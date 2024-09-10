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
  #:use-module ((rnrs base) #:select (assertion-violation (error . raise-error)))
  #:use-module ((rnrs conditions) #:select (define-condition-type))
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (web uri)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
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
  #:use-module (ravanan config)
  #:use-module (ravanan glob)
  #:use-module (ravanan monads)
  #:use-module (ravanan propnet)
  #:use-module (ravanan reader)
  #:use-module (ravanan slurm-api)
  #:use-module (ravanan utils)
  #:use-module (ravanan vectors)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work types)
  #:use-module (ravanan work utils)
  #:export (run-command-line-tool
            command-line-tool-scheduler
            check-requirements
            inherit-requirements
            %command-line-tool-supported-requirements
            scheduler-proc
            scheduler-proc-name
            scheduler-proc-cwl
            scheduler-proc-scatter
            scheduler-proc-scatter-method))

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

;; In batch systems that require it, poll job completion status every
;; 5 seconds.
(define %job-poll-interval
  5)

(define (warning fmt . args)
  (apply format (current-error-port) fmt args)
  (newline))

(define (error fmt . args)
  (apply warning fmt args)
  (exit #f))

(define-immutable-record-type <scheduler-proc>
  (scheduler-proc name cwl scatter scatter-method)
  scheduler-proc?
  (name scheduler-proc-name)
  (cwl scheduler-proc-cwl)
  (scatter scheduler-proc-scatter)
  (scatter-method scheduler-proc-scatter-method))

(define-immutable-record-type <single-machine-job-state>
  (single-machine-job-state script success?)
  single-machine-job-state?
  (script single-machine-job-state-script)
  (success? single-machine-job-state-success?))

(define-immutable-record-type <slurm-job-state>
  (slurm-job-state script job-id)
  slurm-job-state?
  (script slurm-job-state-script)
  (job-id slurm-job-state-job-id))

(define-condition-type &job-failure &error
  job-failure job-failure?
  (script job-failure-script))

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
                             (error "Requirement ~a not supported"
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
  "Return @code{#t} if @var{str} is a CWL javascript expression. Else, return
@code{#f}."
  (and (string-prefix? "$(" str)
       (string-suffix? ")" str)))

(define (strip-javascript-expression expression)
  "Strip $(…) around javascript @var{expression}."
  (substring expression
             (string-length "$(")
             (- (string-length expression)
                (string-length ")"))))

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
         #$(strip-javascript-expression expression)
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

  (define (collect-bindings inputs+types+bindings)
    (append-map input+type-tree+binding->command-line-binding
                inputs+types+bindings))
  
  (define input+type-tree+binding->command-line-binding
    (match-lambda
      ((input type-tree binding)
       ;; Check type.
       (let* ((type (formal-parameter-type type-tree))
              (matched-type (match-type input type)))
         (unless matched-type
           (raise-error input "Type mismatch" input type))
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
                    (append-map (lambda (input)
                                  (input+type-tree+binding->command-line-binding
                                   (list input
                                         (assoc-ref type-tree "items")
                                         (maybe-assoc-ref (just type-tree)
                                                          "inputBinding"))))
                                (vector->list input))
                    (maybe-assoc-ref binding "itemSeparator"))))
            (else
             (list (command-line-binding
                    position prefix matched-type input %nothing)))))))))
  
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
                           (list (or (assoc-ref inputs id)
                                     (assoc-ref formal-input "default")
                                     'null)
                                 (or (assoc-ref formal-input "type")
                                     (error "Type of input ~a not specified"
                                            id))
                                 (maybe-assoc-ref (just formal-input)
                                                  "inputBinding")))))
                  (vector->list (assoc-ref cwl "inputs")))))
    ;; Sort elements using the assigned sorting keys.
    (lambda (binding1 binding2)
      (< (command-line-binding-position binding1)
         (command-line-binding-position binding2))))))

(define (location->path location)
  "Convert file @var{location} URI to path."
  (if (string-prefix? "/" location)
      ;; Sometimes location is actually a path. In that case, return as is.
      location
      ;; If location is an URI, parse the URI and return the path part.
      (uri-path (string->uri location))))

(define (collect-input-files inputs formal-inputs store)
  "Traverse @var{inputs} and @var{formal-inputs} recursively, intern any files
found into the @var{store} and return a list of all the @code{File} type
objects.

The returned @code{File} type objects are updated with @code{basename},
@code{checksum} and @code{size} fields, and store-interned paths in the
@code{location} and @code{path} fields. The @code{basename} field contains the
basename of the original path, and not the store-interned path."
  (let collect ((inputs (map (lambda (formal-input)
                               (let ((id (assoc-ref formal-input "id")))
                                 (or (assoc-ref inputs id)
                                     (assoc-ref formal-input "default")
                                     'null)))
                             formal-inputs))
                (types (map (lambda (formal-input)
                              (let ((id (assoc-ref formal-input "type")))
                                (or (assoc-ref formal-input "type")
                                    (error "Type of input ~a not specified" id))))
                            formal-inputs)))
    (append-map (lambda (input type-tree)
                  ;; Check type.
                  (let* ((type (formal-parameter-type type-tree))
                         (matched-type (match-type input type)))
                    (unless matched-type
                      (raise-error input "Type mismatch" input type))
                    (cond
                     ;; Recurse over array types.
                     ;; TODO: Implement record and enum types.
                     ((array-type? matched-type)
                      (collect (vector->list input)
                               (make-list (vector-length input)
                                          (assoc-ref type-tree "items"))))
                     ((eq? matched-type 'File)
                      (let* ((location (assoc-ref input "location"))
                             (path (or (and location
                                            (location->path location))
                                       (assoc-ref input "path")))
                             (interned-path (intern-file path store)))
                        (list (assoc-set input
                                         (cons "location" interned-path)
                                         (cons "path" interned-path)
                                         (cons "basename" (basename path))
                                         (cons "checksum" (checksum path))
                                         (cons "size"
                                               (stat:size (stat path)))))))
                     (else (list)))))
                inputs
                types)))

(define (resolve-inputs inputs formal-inputs store)
  "Traverse @var{inputs} and @var{formal-inputs} recursively, intern any
files found into the @var{store} and return a tree of the fully
resolved inputs.

The returned @code{File} type objects are updated with
@code{basename}, @code{checksum} and @code{size} fields, and
store-interned paths in the @code{location} and @code{path}
fields. The @code{basename} field contains the basename of the
original path, and not the store-interned path."
  (define (resolve inputs types)
    (vector-map (lambda (input type-tree)
                  ;; Check type.
                  (let* ((type (formal-parameter-type type-tree))
                         (matched-type (match-type input type)))
                    (unless matched-type
                      (raise-error input "Type mismatch" input type))
                    ;; TODO: Implement record and enum types.
                    (cond
                     ;; Recurse over array types.
                     ((array-type? matched-type)
                      (resolve input
                               (make-vector (vector-length input)
                                            (assoc-ref type-tree "items"))))
                     ;; Intern File type inputs and fully resolve
                     ;; them.
                     ((eq? matched-type 'File)
                      (let* ((path (or (and (assoc-ref input "location")
                                            (location->path (assoc-ref input "location")))
                                       (assoc-ref input "path")))
                             (interned-path (intern-file path store)))
                        (assoc-set input
                                   (cons "location" interned-path)
                                   (cons "path" interned-path)
                                   (cons "basename" (basename path))
                                   (cons "checksum" (checksum path))
                                   (cons "size" (stat:size (stat path))))))
                     ;; Return other input types unchanged.
                     (else input))))
                inputs
                types))
  
  (vector-map->list (lambda (input formal-input)
                      (cons (assoc-ref formal-input "id")
                            input))
                    (resolve (vector-map (lambda (formal-input)
                                           (let ((id (assoc-ref* formal-input "id")))
                                             (or (assoc-ref inputs id)
                                                 (assoc-ref formal-input "default")
                                                 'null)))
                                         formal-inputs)
                             (vector-map (lambda (formal-input)
                                           (let ((id (assoc-ref* formal-input "id")))
                                             (or (assoc-ref formal-input "type")
                                                 (error "Type of input ~a not specified"
                                                        id))))
                                         formal-inputs))
                    formal-inputs))

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
                       #~(expand-file-name #$(basename (assoc-ref value "location"))
                                           inputs-directory))
                      (else
                       (error "Invalid formal input type ~a"
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

(define (intern-file file store)
  "Intern @var{file} into the ravanan @var{store}. Return the interned path."
  (let ((interned-path
         (expand-file-name
          (file-name-join* %store-files-directory
                           (string-append (sha1-hash file)
                                          "-"
                                          (basename file)))
          store)))
    (copy-file file interned-path)
    interned-path))

(define (copy-input-files-gexp inputs)
  "Return a G-expression that copies @code{File} type inputs from @var{inputs} into
@code{inputs-directory} and return a new association list with updated
@code{location} and @code{path} fields.

The returned G-expressions will reference an @code{inputs-directory} variable."
  #~(list->dotted-list
     `#$(map (match-lambda
               ((id . input)
                (list id
                      (let copy-input-files ((input input))
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
                               (copy-file #$(assoc-ref input "location")
                                          path-in-inputs-directory)
                               (assoc-set '#$input
                                          (cons "location" path-in-inputs-directory)
                                          (cons "path" path-in-inputs-directory))))
                         (else input))))))
             inputs)))

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
    (map (lambda (dirent)
           (let ((entry-name (assoc-ref dirent "entryname")))
             (let stage ((entry (assoc-ref dirent "entry")))
               (cond
                ((javascript-expression? entry)
                 (stage
                  (evaluate-javascript %node
                                       (strip-javascript-expression entry))))
                ((string? entry)
                 (list entry-name entry))))))
         (vector->list
          (assoc-ref initial-work-dir-requirement
                     "listing"))))

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
          (raise-error #f "glob output binding not specified"))))

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
                                     (raise-error output-value
                                                  "Type mismatch"
                                                  output-value output-type))
                                   (case matched-type
                                     ((null)
                                      (list))
                                     ((File)
                                      (list (cons output-id
                                                  (canonicalize-file-value output-value))))
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
                                         <>)
                                    '#$stdout-outputs)
                               (map (cut other-output->value
                                         workflow-output-directory
                                         <...>)
                                    '#$(map (cut assoc-ref <> "id")
                                            other-outputs)
                                    '#$(map (cut assoc-ref <> "type")
                                            other-outputs)
                                    '#$(map output-binding-glob
                                            other-outputs)))))
             out
             #:pretty #t)
            (newline out)))))
  
  (maybe-let* ((requirements (maybe-assoc-ref (just cwl) "requirements")))
    (check-requirements requirements %command-line-tool-supported-requirements))
  (maybe-let* ((hints (maybe-assoc-ref (just cwl) "hints")))
    (check-requirements hints %command-line-tool-supported-requirements #t))
  ;; Copy input files and update corresponding input objects.
  (build-gexp-script name
    (let ((requirements (inherit-requirements (or (assoc-ref cwl "requirements")
                                                  #())
                                              (or (assoc-ref cwl "hints")
                                                  #()))))
      (with-imported-modules (source-module-closure '((ravanan work command-line-tool)
                                                      (ravanan glob)
                                                      (guix search-paths))
                                                    #:select? (match-lambda
                                                                (('ravanan work . _) #t)
                                                                ('(ice-9 filesystem) #t)
                                                                (('guix . _) #t)
                                                                (('json . _) #t)
                                                                (_ #f)))
        (with-extensions (list guile-gcrypt)
          #~(begin
              (use-modules (ravanan work command-line-tool)
                           (ravanan work utils)
                           (ravanan glob)
                           (srfi srfi-1)
                           (srfi srfi-26)
                           (ice-9 filesystem)
                           (ice-9 match)
                           (ice-9 threads)
                           (guix search-paths)
                           (json))

              (define (canonicalize-file-value value workflow-output-directory)
                (let* ((path (or (assoc-ref value "location")
                                 (assoc-ref value "path")))
                       (workflow-output-path
                        (expand-file-name (basename path)
                                          workflow-output-directory)))
                  ;; Copy file to the workflow output directory in the store.
                  (copy-file path workflow-output-path)
                  ;; Populate all fields of the File type value.
                  (assoc-set value
                    (cons "location" (string-append "file://"
                                                    workflow-output-path))
                    (cons "path" workflow-output-path)
                    (cons "basename" (basename path))
                    (cons "size" (stat:size (stat path)))
                    (cons "checksum" (checksum path)))))

              (define (path->value path workflow-output-directory)
                (canonicalize-file-value `(("class" . "File")
                                           ("path" . ,path))
                                         workflow-output-directory))

              (define (stdout-output->value workflow-output-directory
                                            stdout-directory
                                            output)
                (cons (assoc-ref output "id")
                      (path->value
                       (if (string=? #$stdout-filename
                                     (file-name-join* stdout-directory "stdout"))
                           ;; If stdout filename is unspecified, rename it to a
                           ;; hash of its contents.
                           (let ((hashed-filename
                                  (file-name-join* stdout-directory
                                                   (sha1-hash #$stdout-filename))))
                             (rename-file #$stdout-filename
                                          hashed-filename)
                             hashed-filename)
                           ;; Else, return the stdout filename as it is.
                           #$stdout-filename)
                       workflow-output-directory)))

              (define (other-output->value workflow-output-directory
                                           output-id output-type-tree glob-pattern)
                (cons output-id
                      ;; TODO: Support all types.
                      (let* ((output-type (formal-parameter-type output-type-tree))
                             (paths (glob glob-pattern))
                             (matched-type (glob-match-type paths output-type)))
                        (unless matched-type
                          (error "Type ~a mismatch for globbed paths ~a"
                                 output-type
                                 paths))
                        ;; Coerce output value into matched type.
                        (let ((output-values (map (cut path->value <> workflow-output-directory)
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
                 (let ((inputs
                        #$(copy-input-files-gexp
                           (resolve-inputs inputs (assoc-ref* cwl "inputs") store)))
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
                             ;; Stage files.
                             ;; We currently support Dirent only. TODO: Support
                             ;; others.
                             (map (match-lambda
                                    ((entry-name entry)
                                     (call-with-input-file entry-name
                                       (cut put-string <> entry))))
                                  '#$(from-maybe
                                      (maybe-bind
                                       (find-requirement requirements
                                                         "InitialWorkDirRequirement")
                                       files-to-stage)
                                      (list)))
                             ;; Actually run the command.
                             #$run-command-gexp
                             ;; Capture outputs.
                             #$capture-outputs-gexp)))
                       #$scratch))
                    #$scratch)))
               #$scratch)))))
    guix-daemon-socket))

(define* (command-line-tool-scheduler manifest scratch store batch-system
                                      #:key guix-daemon-socket
                                      slurm-api-endpoint slurm-jwt)
  (define (schedule proc inputs)
    "Schedule @var{proc} with inputs from the @var{inputs} association list. Return a
job state object."
    (let ((name (scheduler-proc-name proc))
          (cwl (scheduler-proc-cwl proc))
          (scatter (from-maybe (scheduler-proc-scatter proc)
                               #f))
          (scatter-method (from-maybe (scheduler-proc-scatter-method proc)
                                      #f)))
      (if scatter
          (case scatter-method
            ((dot-product)
             (apply vector-map
                    (lambda input-elements
                      ;; Recurse with scattered inputs spliced in.
                      (schedule (scheduler-proc (scheduler-proc-name proc)
                                                (scheduler-proc-cwl proc)
                                                %nothing
                                                %nothing)
                                ;; Replace scattered inputs with single
                                ;; elements.
                                (apply assoc-set
                                       inputs
                                       (map cons
                                            (vector->list scatter)
                                            input-elements))))
                    ;; Extract values of scattered inputs.
                    (vector-map->list (cut assoc-ref inputs <>)
                                      scatter)))
            ((nested-cross-product flat-cross-product)
             (raise-error scatter-method
                          "Scatter method not implemented yet")))
          (run-command-line-tool name
                                 manifest
                                 cwl
                                 inputs
                                 scratch
                                 store
                                 batch-system
                                 #:guix-daemon-socket guix-daemon-socket
                                 #:slurm-api-endpoint slurm-api-endpoint
                                 #:slurm-jwt slurm-jwt))))

  (define (poll state)
    "Return current status of job @var{state} object---one of the symbols
@code{pending} or @code{completed}. Raise an exception and exit if job has
failed."
    (guard (c ((job-failure? c)
               (let ((script (job-failure-script c)))
                 (error
                  "~a failed; logs at ~a and ~a~%"
                  script
                  (script->store-stdout-file script store)
                  (script->store-stderr-file script store)))))
      (cond
       ;; Single machine jobs are run synchronously. So, they return success or
       ;; failure immediately.
       ((single-machine-job-state? state)
        (if (single-machine-job-state-success? state)
            'completed
            (raise-exception
             (job-failure (single-machine-job-state-script state)))))
       ;; Poll slurm for job state.
       ((slurm-job-state? state)
        (case (job-state (slurm-job-state-job-id state)
                         #:api-endpoint slurm-api-endpoint
                         #:jwt slurm-jwt)
          ((failed)
           (raise-exception
            (job-failure (slurm-job-state-script state))))
          (else => identity)))
       ;; For vector states, poll each state element and return 'completed only
       ;; if all state elements have completed.
       ((vector? state)
        (or (vector-every (lambda (state-element)
                            (case (poll state-element)
                              ((completed) => identity)
                              (else #f)))
                          state)
            'pending)))))

  (define (capture-output state)
    "Return output of completed job @var{state}."
    (if (vector? state)
        ;; Combine outputs from individual state elements.
        (match (vector-map capture-output state)
          ((and #(head-output _ ...)
                outputs)
           (map (match-lambda
                  ((id . value)
                   (cons id
                         (vector-map (lambda (output)
                                       ;; FIXME: Is this the correct way to
                                       ;; handle missing outputs?
                                       (or (assoc-ref output id)
                                           'null))
                                     outputs))))
                head-output)))
        ;; Log progress and return captured output.
        (let ((script ((case batch-system
                         ((single-machine) single-machine-job-state-script)
                         ((slurm-api) slurm-job-state-script))
                       state)))
          (format (current-error-port)
                  "~a completed; logs at ~a and ~a~%"
                  script
                  (script->store-stdout-file script store)
                  (script->store-stderr-file script store))
          (capture-command-line-tool-output script store))))

  (scheduler schedule
             poll
             (case batch-system
               ;; Single machine jobs are run synchronously. So, there is no
               ;; need to wait to poll them.
               ((single-machine) 0)
               ((slurm-api) %job-poll-interval))
             capture-output))
