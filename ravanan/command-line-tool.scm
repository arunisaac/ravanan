;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of ravanan.
;;;
;;; ravanan is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ravanan is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ravanan.  If not, see <https://www.gnu.org/licenses/>.

(define-module (ravanan command-line-tool)
  #:use-module ((rnrs base) #:select (assertion-violation error))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages guile-xyz) #:select (guile-filesystem))
  #:use-module ((gnu packages node) #:select (node))
  #:use-module (guix describe)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (json)
  #:use-module (yaml)
  #:use-module (ravanan glob)
  #:use-module (ravanan job-state)
  #:use-module (ravanan reader)
  #:use-module (ravanan slurm-api)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work types)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (run-command-line-tool
            check-requirements
            inherit-requirements
            %command-line-tool-supported-requirements
            script->store-stdout-file
            script->store-stderr-file
            capture-command-line-tool-output

            %store-files-directory
            %store-data-directory
            %store-logs-directory))

(define %store-files-directory
  "files")

(define %store-data-directory
  "data")

(define %store-logs-directory
  "logs")

(define %command-line-tool-supported-requirements
  (list "EnvVarRequirement"
        "InlineJavascriptRequirement"
        "InitialWorkDirRequirement"))

;; node executable for evaluating javascript on worker nodes
(define %worker-node
  (file-append node "/bin/node"))

(define-immutable-record-type <formal-output>
  (formal-output id type binding)
  formal-output?
  (id formal-output-id)
  (type formal-output-type)
  (secondary-files formal-output-secondary-files)
  (binding formal-output-binding))

(define-immutable-record-type <command-line-binding>
  (command-line-binding position prefix type value item-separator)
  command-line-binding?
  (position command-line-binding-position)
  (prefix command-line-binding-prefix)
  (type command-line-binding-type)
  (value command-line-binding-value)
  (item-separator command-line-binding-item-separator))

(define-immutable-record-type <output-binding>
  (output-binding glob load-contents? load-listing? output-eval)
  output-binding?
  (glob output-binding-glob)
  (load-contents? output-binding-load-contents?)
  (load-listing? output-binding-load-listing?)
  (output-eval output-binding-output-eval))

(define* (check-requirements requirements supported-requirements
                             #:optional hint?)
  "Error out if any of @var{requirements} are not in
@var{supported-requirements}. If @var{hint?} is @code{#t}, only print a warning."
  (vector-for-each (lambda (requirement)
                     (let ((class (assoc-ref requirement "class")))
                       (unless (member class supported-requirements)
                         (if hint?
                             (warning "Ignoring ~a hint; it is not supported~%"
                                      class)
                             (user-error "Requirement ~a not supported"
                                         class)))))
                   requirements))

(define (inherit-requirements requirements supplementary-requirements)
  "Inherit requirements from @{supplementary-requirements} onto
@var{requirements}. Requirements in @var{requirements} take higher precedence."
  (define (requirement=? requirement1 requirement2)
    "Return @code{#t} if @var{requirement1} and @var{requirement2} are the same
class. Else, return @code{#f}."
    (string=? (assoc-ref* requirement1 "class")
              (assoc-ref* requirement2 "class")))
  
  (vector-append requirements
                 ;; Remove any supplementary requirement that is in
                 ;; requirements.
                 (vector-remove (lambda (supplementary-requirement)
                                  (vector-any (cut requirement=?
                                                   supplementary-requirement
                                                   <>)
                                              requirements))
                                supplementary-requirements)))

(define (javascript-expression? str)
  "Return @code{#t} if @var{str} contains a CWL javascript expression. Else, return
@code{#f}."
  (string-contains str "$("))

(define (interpolate-parameter-references str)
  "Interpolate @var{str} with one or more parameter references into a javascript
expression suitable for evaluation."
  (define (tokenize str)
    "Split @var{str} into alternating tokens of parameter reference and literal
strings."
    (let ((end (if (string-prefix? "$(" str)
                   (1+ (string-index str #\)))
                   (string-index str #\$))))
      (if end
          (cons (substring str 0 end)
                (tokenize (substring str end)))
          (if (string-null? str)
              (list)
              (list str)))))

  (string-join (map (lambda (token)
                      (if (and (string-prefix? "$(" token)
                               (string-suffix? ")" token))
                          ;; Strip $(…).
                          (substring token
                                     (string-length "$(")
                                     (- (string-length token)
                                        (string-length ")")))
                          ;; Surround with double quotes.
                          (string-append "\"" token "\"")))
                    (tokenize str))
               " + "))

(define (coerce-expression expression)
  "Coerce @var{expression} into a scheme JSON tree.

When @var{expression} is a scheme JSON tree, return it as is. When
@var{expression} is a javascript expression, return a G-expression that
evaluates it. This G-expression references variables @code{inputs} and
@code{runtime}."
  (if (and (string? expression)
           (javascript-expression? expression))
      #~(evaluate-parameter-reference
         #$%worker-node
         #$(interpolate-parameter-references expression)
         inputs
         'null
         runtime
         (list))
      expression))

(define (build-command cwl inputs)
  "Return a list of @code{<command-line-binding>} objects for the
@code{CommandLineTool} class workflow @var{cwl} and @var{inputs}. The
@code{value} field of the returned @code{<command-line-binding>} objects may be
strings or G-expressions. The G-expressions may reference @var{inputs} and
@var{runtime} variables that must be defined in the context in which the
G-expressions are inserted."
  (define (argument->command-line-binding i argument)
    (command-line-binding (cond
                           ((assoc-ref argument "position")
                            => string->number)
                           (else i))
                          (maybe-assoc-ref (just argument) "prefix")
                          'string
                          #~(value->string
                             #$(coerce-expression
                                (assoc-ref* argument "valueFrom")))
                          %nothing))

  (define (collect-bindings ids+inputs+types+bindings)
    (append-map id+input+type-tree+binding->command-line-binding
                ids+inputs+types+bindings))
  
  (define id+input+type-tree+binding->command-line-binding
    (match-lambda
      ;; We stretch the idea of an input id, by making it an address that
      ;; identifies the exact location of a value in a tree that possibly
      ;; contains array types. For example, '("foo") identifies the input "foo";
      ;; '("foo" 1) identifies the 1th element of the array input "foo"; '("foo"
      ;; 37 1) identifies the 1th element of the 37th element of the array input
      ;; "foo"; etc.
      ((id input type-tree binding)
       ;; Check type.
       (let* ((type (formal-parameter-type type-tree))
              (matched-type (match-type input type)))
         (unless matched-type
           (error input "Type mismatch" input type))
         (let ((position
                (from-maybe
                 (maybe-let* ((position (maybe-assoc-ref binding "position")))
                   (just (string->number position)))
                 ;; FIXME: Why a default value of 0?
                 0))
               (prefix (maybe-assoc-ref binding "prefix")))
           (cond
            ;; Recurse over array types.
            ;; TODO: Implement record and enum types.
            ((array-type? matched-type)
             (list (command-line-binding
                    position
                    prefix
                    matched-type
                    (append-map (lambda (i input)
                                  (id+input+type-tree+binding->command-line-binding
                                   (list (append id (list i))
                                         input
                                         (assoc-ref type-tree "items")
                                         (maybe-assoc-ref (just type-tree)
                                                          "inputBinding"))))
                                (iota (vector-length input))
                                (vector->list input))
                    (maybe-assoc-ref binding "itemSeparator"))))
            (else
             (list (command-line-binding position
                                         prefix
                                         matched-type
                                         ;; We defer access of input values to
                                         ;; runtime when inputs have been fully
                                         ;; resolved, staging is complete, etc.
                                         #~(apply json-ref inputs '#$id)
                                         %nothing)))))))))
  
  ;; For details of this algorithm, see §4.1 Input binding of the CWL
  ;; 1.2 CommandLineTool specification:
  ;; https://www.commonwl.org/v1.2/CommandLineTool.html#Input_binding
  (append
   ;; Insert elements from baseCommand.
   (vector->list (or (assoc-ref cwl "baseCommand")
                     (vector)))
   (sort
    (append
     ;; Collect CommandLineBinding objects from arguments; assign a sorting key.
     (vector->list
      (vector-map-indexed argument->command-line-binding
                          (or (assoc-ref cwl "arguments")
                              #())))
     ;; Collect CommandLineBinding objects from the inputs schema; assign a
     ;; sorting key.
     (collect-bindings
      (filter-map (lambda (formal-input)
                    ;; Exclude formal inputs without an inputBinding.
                    (and (assoc "inputBinding" formal-input)
                         (let ((id (assoc-ref formal-input "id")))
                           (list (list id)
                                 (or (assoc-ref inputs id)
                                     (assoc-ref formal-input "default")
                                     'null)
                                 (or (assoc-ref formal-input "type")
                                     (user-error "Type of input ~a not specified"
                                                 id))
                                 (maybe-assoc-ref (just formal-input)
                                                  "inputBinding")))))
                  (vector->list (assoc-ref cwl "inputs")))))
    ;; Sort elements using the assigned sorting keys.
    (lambda (binding1 binding2)
      (< (command-line-binding-position binding1)
         (command-line-binding-position binding2))))))

(define (command-line-binding->args binding)
  "Return a list of arguments for @var{binding}. The returned list may
contain strings or G-expressions. The G-expressions may reference an
@code{inputs-directory} variable that must be defined in the context
in which the G-expressions are inserted."
  (let ((prefix (command-line-binding-prefix binding))
        (type (command-line-binding-type binding))
        (value (command-line-binding-value binding)))
    (cond
     ((eq? type 'boolean)
      (if value
          ;; TODO: Error out if boolean input has no prefix?
          (maybe->list prefix)
          (list)))
     ((eq? type 'null) (list))
     ((array-type? type)
      (match value
        ;; Empty arrays should be noops.
        (() (list))
        (_
         (let ((args (append-map command-line-binding->args
                                 value)))
           (append (maybe->list prefix)
                   (from-maybe
                    (maybe-let* ((item-separator (command-line-binding-item-separator binding)))
                      (just (list (string-join args item-separator))))
                    args))))))
     (else
      (append (maybe->list prefix)
              (list (case type
                      ((string)
                       value)
                      ((int)
                       (number->string value))
                      ((File)
                       #~(assoc-ref* #$value "path"))
                      (else
                       (user-error "Invalid formal input type ~a"
                                   type)))))))))

(define* (build-gexp-script name exp #:optional guix-daemon-socket)
  "Build script named @var{name} using G-expression @var{exp}.

When @var{guix-daemon-socket} is provided, connect to that Guix daemon."
  (if guix-daemon-socket
      (parameterize ((%daemon-socket-uri guix-daemon-socket))
        (build-gexp-script name exp))
      (with-store store
        (run-with-store store
          (mlet %store-monad ((drv (gexp->script name exp)))
            (mbegin %store-monad
              (built-derivations (list drv))
              (return (derivation->output-path drv))))))))

(define (script->store-files-directory script store)
  "Return the store files directory in @var{store} corresponding to @var{script}
path."
  (expand-file-name (file-name-join* %store-files-directory
                                     (basename script))
                    store))

(define (script->store-data-file script store)
  "Return the store data file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-data-directory
                                     (string-append (basename script) ".json"))
                    store))

(define (script->store-stdout-file script store)
  "Return the store stdout file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append (basename script) ".stdout"))
                    store))

(define (script->store-stderr-file script store)
  "Return the store stderr file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append (basename script) ".stderr"))
                    store))

(define* (run-command-line-tool name manifest cwl inputs
                                scratch store batch-system
                                #:key guix-daemon-socket
                                slurm-api-endpoint slurm-jwt)
  "Run @code{CommandLineTool} class workflow @var{cwl} named @var{name} with
@var{inputs} using tools from Guix @var{manifest}.

@var{scratch}, @var{store}, @var{batch-system}, @var{guix-daemon-socket},
@var{slurm-api-endpoint} and @var{slurm-jwt} are the same as in
@code{run-workflow} from @code{(ravanan workflow)}."
  ;; TODO: Write to the store atomically.
  (let* ((script
          (build-command-line-tool-script name manifest cwl inputs
                                          scratch store guix-daemon-socket))
         (store-files-directory (script->store-files-directory script store))
         (store-data-file (script->store-data-file script store))
         (stdout-file (script->store-stdout-file script store))
         (stderr-file (script->store-stderr-file script store)))
    (if (file-exists? store-data-file)
        ;; Return a dummy success state object if script has already
        ;; been run successfully.
        (begin
          (format (current-error-port)
                  "~a previously run; retrieving result from store~%"
                  script)
          (single-machine-job-state script #t))
        ;; Run script if it has not already been run.
        (begin
          ;; Delete output files directory if an incomplete one exists
          ;; from a previous crashed run. Purely computationally, a
          ;; run that failed earlier should not be able to succeed
          ;; when run again. But, this may yet happen when runs crash
          ;; due to externalities (think power cut, filesystem issues,
          ;; etc.).
          (when (file-exists? store-files-directory)
            (delete-file-recursively store-files-directory))
          (mkdir store-files-directory)
          (case batch-system
            ((single-machine)
             (setenv "WORKFLOW_OUTPUT_DIRECTORY" store-files-directory)
             (setenv "WORKFLOW_OUTPUT_DATA_FILE" store-data-file)
             (format (current-error-port)
                     "Running ~a~%"
                     script)
             (single-machine-job-state script
                                       (zero? (with-output-to-file stdout-file
                                                (lambda ()
                                                  (with-error-to-file stderr-file
                                                    (cut system* script)))))))
            ((slurm-api)
             (format (current-error-port)
                     "Submitting job ~a~%"
                     script)
             (let ((job-id (submit-job `(("WORKFLOW_OUTPUT_DIRECTORY" .
                                          ,store-files-directory)
                                         ("WORKFLOW_OUTPUT_DATA_FILE" .
                                          ,store-data-file))
                                       stdout-file
                                       stderr-file
                                       name
                                       script
                                       #:api-endpoint slurm-api-endpoint
                                       #:jwt slurm-jwt)))
               (format (current-error-port)
                       "~a submitted as job ID ~a~%"
                       script
                       job-id)
               (slurm-job-state script job-id)))
            (else
             (assertion-violation batch-system "Invalid batch system")))))))

(define (capture-command-line-tool-output script store)
  "Capture and return output of @code{CommandLineTool} class workflow that ran
@var{script}. @var{store} is the path to the ravanan store."
  (let* ((store-data-file (script->store-data-file script store))
         (output-json (call-with-input-file store-data-file
                        json->scm)))
    ;; Recursively rewrite file paths in output JSON.
    (call-with-output-file store-data-file
      (lambda (port)
        (scm->json (let rewrite ((tree output-json))
                     (cond
                      ;; Arrays
                      ((vector? tree)
                       (vector-map rewrite tree))
                      ;; Files
                      ((and (list? tree)
                            (assoc-ref tree "class")
                            (string=? (assoc-ref tree "class")
                                      "File"))
                       (let* ((store-files-directory
                               (script->store-files-directory script store))
                              (path (expand-file-name
                                     (relative-file-name (assoc-ref tree "path")
                                                         store-files-directory)
                                     store-files-directory)))
                         (assoc-set tree
                                    (cons "location" (string-append "file://" path))
                                    (cons "path" path))))
                      ;; Dictionaries
                      ((list? tree)
                       (map (match-lambda
                              ((key . value)
                               (cons key (rewrite value))))
                            tree))
                      ;; Literals
                      (else tree)))
                   port
                   #:pretty #t)
        (newline port)))
    ;; Return output values.
    (call-with-input-file store-data-file
      json->scm)))

(define (copy-input-files-gexp inputs)
  "Return a G-expression that copies @code{File} type inputs (along with secondary
files) from @var{inputs} into @code{inputs-directory} and return a new
association list with updated @code{location} and @code{path} fields.

The returned G-expression will reference an @code{inputs-directory} variable."
  (define (copy-input-files input)
    (cond
     ((vector? input)
      #~,(list->vector
          `#$(map copy-input-files
                  (vector->list input))))
     ((eq? (object-type input)
           'File)
      #~,(let ((path-in-inputs-directory
                (expand-file-name #$(assoc-ref input "basename")
                                  inputs-directory)))
           (copy-file #$(assoc-ref input "path")
                      path-in-inputs-directory)
           (maybe-assoc-set '#$input
             (cons "location"
                   (just path-in-inputs-directory))
             (cons "path"
                   (just path-in-inputs-directory))
             (cons "secondaryFiles"
                   #$(from-maybe
                      (maybe-let* ((secondary-files
                                    (maybe-assoc-ref (just input) "secondaryFiles")))
                        (just #~(just (list->vector
                                       `#$(vector-map->list copy-input-files
                                                            secondary-files)))))
                      #~%nothing)))))
     (else input)))

  #~(list->dotted-list
     `#$(map (match-lambda
               ((id . input)
                (list id (copy-input-files input))))
             inputs)))

(define (canonicalize-json tree)
  "Canonicalize JSON @var{tree} by recursively sorting objects in lexicographic
order of keys."
  ;; We need to canonicalize JSON trees before inserting them into
  ;; G-expressions. If we don't, we would have degenerate G-expressions that
  ;; produce exactly the same result.
  (cond
   ;; Sort objects by lexicographic order of keys, and recurse.
   ((list? tree)
    (sort (map (match-lambda
                 ((key . value)
                  (cons key (canonicalize-json value))))
               tree)
          (match-lambda*
            (((key1 . _) (key2 . _))
             (string< key1 key2)))))
   ;; Do not rearrange arrays. Just recurse.
   ((vector? tree)
    (vector-map canonicalize-json tree))
   ;; Atoms
   (else tree)))

(define (build-command-line-tool-script name manifest cwl inputs
                                        scratch store guix-daemon-socket)
  "Build and return script to run @code{CommandLineTool} class workflow @var{cwl}
named @var{name} with @var{inputs} using tools from Guix manifest
@var{manifest}.

@var{scratch}, @var{store} and @var{guix-daemon-socket} are the same as in
@code{run-workflow} from @code{(ravanan workflow)}."
  (define (find-requirement requirements class)
    (maybe-vector-find (lambda (requirement)
                         (string=? (assoc-ref* requirement "class")
                                   class))
                       requirements))
  
  (define (environment-variables env-var-requirement)
    (just (vector-map->list (lambda (environment-definition)
                              #~(list #$(assoc-ref* environment-definition
                                                    "envName")
                                      #$(coerce-expression
                                         (assoc-ref* environment-definition
                                                     "envValue"))))
                            (assoc-ref* env-var-requirement "envDef"))))

  (define (files-to-stage initial-work-dir-requirement)
    (vector-map->list (lambda (listing-entry)
                        (if (string? listing-entry)
                            ;; File
                            #~(let ((entry #$(coerce-expression listing-entry)))
                                (list (assoc-ref entry "basename")
                                      entry))
                            ;; Dirent
                            #~(list #$(coerce-expression
                                       (assoc-ref listing-entry "entryname"))
                                    #$(coerce-expression
                                       (assoc-ref listing-entry "entry")))))
                      (assoc-ref initial-work-dir-requirement
                                 "listing")))

  (define stdout-filename
    (cond
     ;; stdout filename or expression is specified.
     ((assoc-ref cwl "stdout") => coerce-expression)
     ;; stdout filename is not specified, but one of the outputs is of type
     ;; stdout.
     ((vector-any (lambda (output)
                    (equal? (assoc-ref* output "type")
                            "stdout"))
                  (assoc-ref* cwl "outputs"))
      #~(file-name-join* stdout-directory "stdout"))
     (else #f)))

  (define (output-binding-glob output)
    (from-maybe
     (maybe-assoc-ref (just output)
                      "outputBinding" "glob")
     (and (memq (formal-parameter-type (assoc-ref* output "type"))
                (list 'File 'Directory))
          (error #f "glob output binding not specified"))))

  (define run-command-gexp
    #~(run-command (list #$@(append-map (lambda (arg)
                                          (if (command-line-binding? arg)
                                              (command-line-binding->args arg)
                                              (list arg)))
                                        (build-command cwl inputs)))
                   #$(coerce-expression (assoc-ref cwl "stdin"))
                   #$stdout-filename
                   '#$(from-maybe
                       (maybe-let* ((success-codes
                                     (maybe-assoc-ref (just cwl) "successCodes")))
                         (just (map string->number
                                    (vector->list success-codes))))
                       (list 0))))

  (define capture-outputs-gexp
    #~(let ((workflow-output-directory (getenv "WORKFLOW_OUTPUT_DIRECTORY")))
        (call-with-output-file (getenv "WORKFLOW_OUTPUT_DATA_FILE")
          (lambda (out)
            (scm->json
             (if (file-exists? "cwl.output.json")
                 ;; TODO: Check types. Filter.
                 (let ((outputs (call-with-input-file "cwl.output.json"
                                  json->scm)))
                   ;; TODO: Warn if dropping any of the outputs.
                   (append-map (lambda (output)
                                 (let* ((output-id (assoc-ref output "id"))
                                        (output-type (formal-parameter-type
                                                      (assoc-ref output "type")))
                                        (output-value (or (assoc-ref outputs output-id)
                                                          'null))
                                        (matched-type (match-type output-value output-type)))
                                   (unless matched-type
                                     (error output-value
                                            "Type mismatch"
                                            output-value output-type))
                                   (case matched-type
                                     ((null)
                                      (list))
                                     ((File)
                                      (list (cons output-id
                                                  (copy-file-value
                                                   (canonicalize-file-value output-value)
                                                   workflow-output-directory))))
                                     (else
                                      (list (cons output-id output-value))))))
                               '#$(vector->list (assoc-ref cwl "outputs"))))
                 #$(let ((stdout-outputs other-outputs
                                         (partition (lambda (output)
                                                      (equal? (assoc-ref* output "type")
                                                              "stdout"))
                                                    (vector->list (assoc-ref* cwl "outputs")))))
                     #~(append (map (cut stdout-output->value
                                         workflow-output-directory
                                         stdout-directory
                                         #$stdout-filename
                                         <>)
                                    '#$stdout-outputs)
                               (map (cut other-output->value
                                         workflow-output-directory
                                         <...>)
                                    '#$(map (cut assoc-ref <> "id")
                                            other-outputs)
                                    '#$(map (cut assoc-ref <> "type")
                                            other-outputs)
                                    (list #$@(map (lambda (output)
                                                    (match (assoc "secondaryFiles" output)
                                                      ((_ . secondary-files)
                                                       #~(just #$secondary-files))
                                                      (#f #~%nothing)))
                                                  other-outputs))
                                    (list #$@(map (compose coerce-expression
                                                           output-binding-glob)
                                                  other-outputs))))))
             out
             #:pretty #t)
            (newline out)))))
  
  (maybe-let* ((requirements (maybe-assoc-ref (just cwl) "requirements")))
    (check-requirements requirements %command-line-tool-supported-requirements))
  (maybe-let* ((hints (maybe-assoc-ref (just cwl) "hints")))
    (check-requirements hints %command-line-tool-supported-requirements #t))
  ;; Copy input files and update corresponding input objects.
  (build-gexp-script name
    (let* ((requirements (inherit-requirements (or (assoc-ref cwl "requirements")
                                                   #())
                                               (or (assoc-ref cwl "hints")
                                                   #())))
           (initial-work-dir-requirement (find-requirement requirements
                                                           "InitialWorkDirRequirement")))
      (with-imported-modules (source-module-closure '((ravanan work command-line-tool)
                                                      (ravanan work monads)
                                                      (ravanan work ui)
                                                      (ravanan work vectors)
                                                      (ravanan glob)
                                                      (guix search-paths))
                                                    #:select? (match-lambda
                                                                (('ravanan work . _) #t)
                                                                (('guix . _) #t)
                                                                (('json . _) #t)
                                                                (_ #f)))
        (with-extensions (list guile-filesystem guile-gcrypt)
          #~(begin
              (use-modules (ravanan work command-line-tool)
                           (ravanan work monads)
                           (ravanan work ui)
                           (ravanan work utils)
                           (ravanan work vectors)
                           (ravanan glob)
                           (rnrs io ports)
                           (srfi srfi-1)
                           (srfi srfi-26)
                           (ice-9 filesystem)
                           (ice-9 match)
                           (ice-9 threads)
                           (guix search-paths)
                           (json))

              (define (copy-file-value value directory)
                ;; Copy file represented by value to directory and return the
                ;; new File value.
                (let* ((path (assoc-ref* value "path"))
                       (destination-path (expand-file-name (basename path)
                                                           directory)))
                  (copy-file path destination-path)
                  (assoc-set value
                    (cons "location" (string-append "file://" destination-path))
                    (cons "path" destination-path))))

              (define (canonicalize-file-value value)
                (let ((path (or (assoc-ref value "location")
                                (assoc-ref value "path"))))
                  ;; Populate all fields of the File type value.
                  `(("class" . "File")
                    ("location" . ,(string-append "file://" path))
                    ("path" . ,path)
                    ("basename" . ,(basename path))
                    ("nameroot" . ,(file-name-stem path))
                    ("nameext" . ,(file-name-extension path))
                    ("size" . ,(stat:size (stat path)))
                    ("checksum" . ,(checksum path)))))

              (define (capture-secondary-file path secondary-file
                                              workflow-output-directory)
                "Capture @var{secondary-file} for primary @var{path} and return its
canonicalized value. If @var{required} is @code{#t} and no such secondary file
is found, error out. @var{workflow-output-directory} is path to the output
directory of the workflow."
                (let* ((pattern (assoc-ref* secondary-file "pattern"))
                       ;; TODO: Implement caret characters in
                       ;; SecondaryFileSchema DSL.
                       (secondary-file-path (string-append path pattern))
                       (secondary-file-value
                        (and (file-exists? secondary-file-path)
                             (copy-file-value (canonicalize-file-value
                                               `(("class" . "File")
                                                 ("path" . ,secondary-file-path)))
                                              workflow-output-directory))))
                  (if (and (assoc-ref* secondary-file "required")
                           (not secondary-file-value))
                      (user-error "Secondary file ~a missing for output path ~a"
                                  pattern
                                  path)
                      secondary-file-value)))

              (define (path->value path workflow-output-directory maybe-secondary-files)
                (maybe-assoc-set (copy-file-value (canonicalize-file-value
                                                   `(("class" . "File")
                                                     ("path" . ,path)))
                                                  workflow-output-directory)
                  (cons "secondaryFiles"
                        (maybe-let* ((secondary-files maybe-secondary-files))
                          (just (vector-filter-map (cut capture-secondary-file
                                                        path
                                                        <>
                                                        workflow-output-directory)
                                                   secondary-files))))))

              (define (stdout-output->value workflow-output-directory
                                            stdout-directory
                                            stdout-filename
                                            output)
                (cons (assoc-ref output "id")
                      (path->value
                       (if (string=? stdout-filename
                                     (file-name-join* stdout-directory "stdout"))
                           ;; If stdout filename is unspecified, rename it to a
                           ;; hash of its contents.
                           (let ((hashed-filename
                                  (file-name-join* stdout-directory
                                                   (sha1-hash stdout-filename))))
                             (rename-file stdout-filename
                                          hashed-filename)
                             hashed-filename)
                           ;; Else, return the stdout filename as it is.
                           stdout-filename)
                       workflow-output-directory
                       %nothing)))

              (define (other-output->value workflow-output-directory
                                           output-id output-type-tree
                                           maybe-secondary-files glob-pattern)
                (cons output-id
                      ;; TODO: Support all types.
                      (let* ((output-type (formal-parameter-type output-type-tree))
                             (paths (glob glob-pattern))
                             (matched-type (glob-match-type paths output-type)))
                        (unless matched-type
                          (user-error "Type ~a mismatch for globbed paths ~a"
                                      output-type
                                      paths))
                        ;; Coerce output value into matched type.
                        (let ((output-values (map (cut path->value
                                                       <>
                                                       workflow-output-directory
                                                       maybe-secondary-files)
                                                  paths)))
                          (cond
                           ((memq matched-type (list 'File 'Directory))
                            (match output-values
                              ((output-file)
                               output-file)))
                           ;; TODO: Recurse.
                           ((and (array-type? matched-type)
                                 (memq (array-type-subtype matched-type)
                                       (list 'File 'Directory)))
                            (list->vector output-values))
                           ((eq? matched-type 'null)
                            'null))))))

              ;; Stage files.
              ;; We currently support File and Dirent only. TODO: Support others.
              (define (stage-files entries outputs-directory)
                ;; Stage entries and return an association list mapping files
                ;; (presumably input files) that were staged.
                (filter-map (match-lambda
                              ((entry-name entry)
                               (cond
                                ;; Stuff string literal into a file.
                                ((string? entry)
                                 (call-with-input-file entry-name
                                   (cut put-string <> entry))
                                 #f)
                                ;; Symlink to the file.
                                ((eq? (object-type entry)
                                      'File)
                                 ;; TODO: Stage secondary files too?
                                 (rename-file (assoc-ref* entry "path")
                                              entry-name)
                                 (cons entry
                                       (canonicalize-file-value
                                        `(("class" . "File")
                                          ("path" . ,entry-name))))))))
                            entries))

              (define (set-staged-path input staging-mapping)
                ;; If input is a File type input that was staged, return new
                ;; staged value. Else, return as is.
                (cond
                 ;; Recurse on vector inputs.
                 ((vector? input)
                  (list->vector
                   (map (cut set-staged-path <> staging-mapping)
                        (vector->list input))))
                 ;; Try to replace File input value with staged value.
                 ((eq? (object-type input)
                       'File)
                  (or (any (match-lambda
                             ((old-value . new-value)
                              (and (alist=? input old-value)
                                   new-value)))
                           staging-mapping)
                      input))
                 ;; Else, return as is.
                 (else input)))

              ;; Set search paths for manifest.
              (for-each (match-lambda
                          ((specification . value)
                           (setenv (search-path-specification-variable specification)
                                   value)))
                        (evaluate-search-paths
                         (map sexp->search-path-specification
                              '#$(map search-path-specification->sexp
                                      (manifest-search-paths manifest)))
                         (list #$(profile
                                  (content manifest)
                                  (allow-collisions? #t)))))

              (call-with-temporary-directory
               (lambda (inputs-directory)
                 (let ((inputs #$(copy-input-files-gexp (canonicalize-json inputs)))
                       (runtime `(("cores" . ,(total-processor-count)))))

                   ;; Set environment defined by workflow.
                   (for-each (match-lambda
                               ((name value)
                                (setenv name value)))
                             (list #$@(from-maybe
                                       (maybe-bind
                                        (find-requirement requirements
                                                          "EnvVarRequirement")
                                        environment-variables)
                                       (list))))

                   (call-with-temporary-directory
                    (lambda (stdout-directory)
                      (call-with-temporary-directory
                       (lambda (outputs-directory)
                         (call-with-current-directory outputs-directory
                           (lambda ()
                             (let* ((staging-mapping
                                     (stage-files (list #$@(from-maybe
                                                            (maybe-bind initial-work-dir-requirement
                                                                        (compose just files-to-stage))
                                                            (list)))
                                                  outputs-directory))
                                    (inputs
                                     (map (match-lambda
                                            ((id . input)
                                             (cons id
                                                   (set-staged-path input
                                                                    staging-mapping))))
                                          inputs)))
                               ;; Actually run the command.
                               #$run-command-gexp
                               ;; Capture outputs.
                               #$capture-outputs-gexp))))
                       #$scratch))
                    #$scratch)))
               #$scratch)))))
    guix-daemon-socket))
