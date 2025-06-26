;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2024, 2025 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages guile-xyz) #:select (guile-filesystem))
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix modules)
  #:use-module ((guix monads) #:select (mlet mbegin return))
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (json)
  #:use-module (ravanan batch-system)
  #:use-module (ravanan javascript)
  #:use-module (ravanan job-state)
  #:use-module (ravanan reader)
  #:use-module ((ravanan single-machine) #:prefix single-machine:)
  #:use-module ((ravanan slurm-api) #:prefix slurm:)
  #:use-module (ravanan store)
  #:use-module (ravanan utils)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work types)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (build-command-line-tool-script
            run-command-line-tool
            check-requirements
            inherit-requirements
            find-requirement
            %command-line-tool-supported-requirements
            command-line-tool-supported-requirements
            capture-command-line-tool-output

            manifest-file-error?
            manifest-file-error-file))

;; &manifest-file-error represents an error loading an user-provided manifest
;; file.
(define-condition-type &manifest-file-error &error
  manifest-file-error manifest-file-error?
  (file manifest-file-error-file))

(define %command-line-tool-supported-requirements
  (list "EnvVarRequirement"
        "InlineJavascriptRequirement"
        "InitialWorkDirRequirement"
        "ResourceRequirement"
        "SoftwareRequirement"
        "WorkReuse"))

(define (command-line-tool-supported-requirements batch-system)
  (cond
    ((eq? batch-system 'single-machine)
     (delete "ResourceRequirement"
             %command-line-tool-supported-requirements))
    ((slurm-api-batch-system? batch-system)
     %command-line-tool-supported-requirements)
    (else
     (assertion-violation batch-system "Unknown batch system"))))

(define-immutable-record-type <formal-output>
  (formal-output id type binding)
  formal-output?
  (id formal-output-id)
  (type formal-output-type)
  (secondary-files formal-output-secondary-files)
  (binding formal-output-binding))

(define-immutable-record-type <output-binding>
  (output-binding glob load-contents? load-listing? output-eval)
  output-binding?
  (glob output-binding-glob)
  (load-contents? output-binding-load-contents?)
  (load-listing? output-binding-load-listing?)
  (output-eval output-binding-output-eval))

(define* (check-requirements requirements
                             batch-system
                             supported-requirements-for-batch-system
                             supported-requirements
                             #:optional hint?)
  "Error out if any of @var{requirements} are not supported by @var{batch-system}.
If @var{hint?} is @code{#t}, only print a warning.
@var{supported-requirements-for-batch-system} is a function that when passed a
batch system returns the requirements supported by it.
@var{supported-requirements} is the list of requirements supported by at least
one batch system."
  (vector-for-each (lambda (requirement)
                     (let ((class (assoc-ref requirement "class")))
                       (unless (member class
                                       (supported-requirements-for-batch-system batch-system))
                         (if hint?
                             (if (member class supported-requirements)
                                 (warning "Ignoring ~a hint; it is not supported for batch system ~a"
                                          class
                                          batch-system)
                                 (warning "Ignoring ~a hint; it is not supported"
                                          class))
                             (if (member class supported-requirements)
                                 (user-error "Requirement ~a not supported for batch system ~a"
                                             class
                                             batch-system)
                                 (user-error "Requirement ~a not supported"
                                             class))))))
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

(define* (coerce-expression expression #:optional context)
  "Coerce @var{expression} into a scheme JSON tree.

When @var{expression} is a scheme JSON tree, return it as is. When
@var{expression} is a javascript expression, return a G-expression that
evaluates it. This G-expression references variables @code{inputs} and
@code{runtime}.

If @var{context} is not @code{#f}, evaluate the parameter reference in that
context and return the value. @var{context} must be an association list with
keys @code{input}, @code{self} and @code{runtime}."
  (if (and (string? expression)
           (javascript-expression? expression))
      ;; Evaluate javascript expression.
      (evaluate-parameter-reference expression context)
      ;; Not a javascript expression, but some other JSON tree. Return it as is.
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

(define* (build-gexp-script name exp store)
  "Build script named @var{name} using G-expression @var{exp}. Connect to the Guix
daemon using @var{store}."
  (run-with-store store
    (mlet %store-monad ((drv (gexp->script name exp)))
      (mbegin %store-monad
        (built-derivations (list drv))
        (return (derivation->output-path drv))))))

(define* (run-command-line-tool name script inputs resource-requirement
                                store batch-system)
  "Run @code{CommandLineTool} class workflow @var{script} named @var{name} with
@var{inputs}. Return a state-monadic job state object.

@var{resource-requirement} is the @code{ResourceRequirement} of the workflow.
@var{store} and @var{batch-system} are the same as in @code{run-workflow} from
@code{(ravanan workflow)}."
  (let ((cpus (from-maybe
               (maybe-bind (maybe-assoc-ref resource-requirement
                                            "coresMin")
                           (compose just
                                    inexact->exact
                                    ceiling
                                    (cut coerce-type <> 'number)
                                    (cut coerce-expression
                                         <>
                                         `(("inputs" . ,inputs)))))
               1))
        (store-files-directory (step-store-files-directory script inputs store))
        (store-data-file (step-store-data-file script inputs store))
        (stdout-file (step-store-stdout-file script inputs store))
        (stderr-file (step-store-stderr-file script inputs store)))
    (if (file-exists? store-data-file)
        ;; Return a dummy success state object if script has already
        ;; been run successfully.
        (state-return
         (begin
           (format (current-error-port)
                   "~a previously run; retrieving result from store~%"
                   script)
           (single-machine-job-state script inputs #t)))
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
          (let ((environment
                 `(("WORKFLOW_INPUTS" . ,(scm->json-string inputs))
                   ("WORKFLOW_OUTPUT_DIRECTORY" . ,store-files-directory)
                   ("WORKFLOW_OUTPUT_DATA_FILE" . ,store-data-file))))
            (cond
             ((eq? batch-system 'single-machine)
              (state-let* ((success? (single-machine:submit-job environment
                                                                stdout-file
                                                                stderr-file
                                                                script)))
                (state-return (single-machine-job-state script inputs success?))))
             ((slurm-api-batch-system? batch-system)
              (state-let* ((job-id
                            (slurm:submit-job environment
                                              stdout-file
                                              stderr-file
                                              cpus
                                              name
                                              script
                                              #:api-endpoint (slurm-api-batch-system-endpoint batch-system)
                                              #:jwt (slurm-api-batch-system-jwt batch-system)
                                              #:partition (slurm-api-batch-system-partition batch-system)
                                              #:nice (slurm-api-batch-system-nice batch-system))))
                (format (current-error-port)
                        "~a submitted as job ID ~a~%"
                        script
                        job-id)
                (state-return (slurm-job-state script inputs job-id))))
             (else
              (assertion-violation batch-system "Invalid batch system"))))))))

(define (capture-command-line-tool-output script inputs store)
  "Capture and return output of @code{CommandLineTool} class workflow that ran
@var{script} with @var{inputs}. @var{store} is the path to the ravanan store."
  (let* ((store-data-file (step-store-data-file script inputs store))
         (output-json (call-with-input-file store-data-file
                        json->scm)))
    ;; Recursively rewrite file paths in output JSON.
    (call-with-atomic-output-file store-data-file
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
                               (step-store-files-directory script inputs store))
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

(define (find-requirement requirements class)
  "Find requirement of @var{class} among @var{requirements} and return a
maybe-monadic value."
  (maybe-vector-find (lambda (requirement)
                       (string=? (assoc-ref* requirement "class")
                                 class))
                     requirements))

(define (load-manifest manifest-file)
  "Load Guix manifest from @var{manifest-file} and return it."
  (if (and manifest-file
           (file-exists? manifest-file))
      ;; Capture conditions raised by load-script and bubble them up along with
      ;; &manifest-file-error.
      (guard (c (else
                 (raise-exception (condition (manifest-file-error manifest-file)
                                             c))))
        (load-script manifest-file
                     #:modules '((guile)
                                 (gnu packages)
                                 (guix gexp)
                                 (guix profiles))))
      (raise-exception (manifest-file-error manifest-file))))

(define (manifest-file->environment manifest-file inferior store)
  "Build @var{manifest-file} and return an association list of environment
variables to set to use the built profile. Connect to the Guix daemon using
@var{store}. Build manifest in @var{inferior} unless it is @code{#f}."
  (if inferior
      (cut inferior-eval
           `(begin
              (use-modules (ice-9 match)
                           (guix search-paths)
                           (gnu packages)
                           (guile)
                           (guix gexp)
                           (guix profiles))

              (define (build-derivation drv)
                (run-with-store store
                  (mlet %store-monad ((drv drv))
                    (mbegin %store-monad
                      (built-derivations (list drv))
                      (return (derivation->output-path drv))))))

              ;; Do not auto-compile manifest files.
              (set! %load-should-auto-compile #f)
              (let ((manifest (load ,(canonicalize-path manifest-file))))
                (map (match-lambda
                       ((specification . value)
                        (cons (search-path-specification-variable specification)
                              value)))
                     (evaluate-search-paths
                      (manifest-search-paths manifest)
                      (list (build-derivation
                             (profile-derivation manifest
                                                 #:allow-collisions? #t)))))))
           <>)
      (manifest->environment (load-manifest manifest-file)
                             store)))

(define (software-packages->environment packages inferior store)
  "Build a profile with @var{packages} and return an association list
of environment variables to set to use the built profile. @var{packages} is a
vector of @code{SoftwarePackage} assocation lists as defined in the CWL
standard. Connect to the Guix daemon using @var{store}. Look up packages in
@var{inferior} unless it is @code{#f}."
  (define (software-package->package-specification package)
    (string-append (assoc-ref* package "package")
                   (from-maybe
                    (maybe-bind (maybe-assoc-ref (just package) "version")
                                (compose just
                                         (cut string-append "@" <>)))
                    "")))

  (define packages->environment
    (compose (cut manifest->environment <> store)
             packages->manifest))

  (if inferior
      (packages->environment
       (vector-map->list (lambda (package)
                           (let ((name (assoc-ref package "package"))
                                 (version (assoc-ref package "version")))
                             (match (lookup-inferior-packages inferior
                                                              name
                                                              version)
                               ((inferior-package _ ...)
                                inferior-package))))
                         packages))
      (packages->environment
       (vector-map->list (compose specification->package
                                  software-package->package-specification)
                         packages))))

(define (manifest->environment manifest store)
  "Build @var{manifest} and return an association list of environment
variables to set to use the built profile. Connect to the Guix daemon using
@var{store}."
  (define (build-derivation drv)
    (run-with-store store
      (mlet %store-monad ((drv drv))
        (mbegin %store-monad
          (built-derivations (list drv))
          (return (derivation->output-path drv))))))

  (map (match-lambda
         ((specification . value)
          (cons (search-path-specification-variable specification)
                value)))
       (evaluate-search-paths
        (manifest-search-paths manifest)
        (list (build-derivation
               (profile-derivation manifest
                                   #:allow-collisions? #t))))))

(define (build-command-line-tool-script name manifest-file inferior cwl
                                        scratch store batch-system
                                        guix-store)
  "Build and return script to run @code{CommandLineTool} class workflow @var{cwl}
named @var{name} using tools from Guix manifest in @var{manifest-file} and on
@var{batch-system}. Use @var{inferior} to build manifests, unless it is
@code{#f}. Connect to the Guix daemon using @var{guix-store}.

@var{scratch} and @var{store} are the same as in @code{run-workflow} from
@code{(ravanan workflow)}."
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

  (define (cores batch-system)
    (cond
     ((slurm-api-batch-system? batch-system)
      #~(string->number (getenv "SLURM_CPUS_ON_NODE")))
     ((eq? batch-system 'single-machine)
      #~(total-processor-count))
     (else
      (assertion-violation batch-system "Unknown batch system"))))

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

  (define (coerce-argument argument)
    (assoc-set argument
      (cons "valueFrom"
            (coerce-expression (assoc-ref* argument "valueFrom")))))

  (define run-command-gexp
    #~(run-command (append-map (lambda (arg)
                                 (if (command-line-binding? arg)
                                     (command-line-binding->args arg)
                                     (list arg)))
                               (build-command #$(assoc-ref cwl "baseCommand")
                                              #$(vector-map coerce-argument
                                                            (assoc-ref cwl "arguments"))
                                              #$(assoc-ref cwl "inputs")
                                              inputs))
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
        (call-with-atomic-output-file (getenv "WORKFLOW_OUTPUT_DATA_FILE")
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
    (check-requirements requirements
                        batch-system
                        command-line-tool-supported-requirements
                        %command-line-tool-supported-requirements)
    ;; Error out if WorkReuse is disabled.
    (maybe-let* ((work-reuse (find-requirement requirements "WorkReuse")))
      (and (not (coerce-type (assoc-ref* work-reuse "enableReuse")
                             'boolean))
           (user-error "Disabling WorkReuse is not supported. With ravanan's strong caching using Guix, there is no need to disable WorkReuse."))))
  (maybe-let* ((hints (maybe-assoc-ref (just cwl) "hints")))
    (check-requirements hints
                        batch-system
                        command-line-tool-supported-requirements
                        %command-line-tool-supported-requirements
                        #t)
    ;; Warn if WorkReuse is disabled.
    (maybe-let* ((work-reuse (find-requirement hints "WorkReuse")))
      (and (not (coerce-type (assoc-ref* work-reuse "enableReuse")
                             'boolean))
           (warning "Ignoring disable of WorkReuse. With ravanan's strong caching using Guix, there is no need to disable WorkReuse."))))
  (build-gexp-script name
    (let* ((requirements (inherit-requirements (or (assoc-ref cwl "requirements")
                                                   #())
                                               (or (assoc-ref cwl "hints")
                                                   #())))
           (initial-work-dir-requirement (find-requirement requirements
                                                           "InitialWorkDirRequirement"))
           (manifest-file
            (from-maybe (maybe-assoc-ref
                         (find-requirement requirements "SoftwareRequirement")
                         "manifest")
                        manifest-file))
           (packages
            (from-maybe (maybe-assoc-ref
                         (find-requirement requirements "SoftwareRequirement")
                         "packages")
                        #())))
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
                           (ravanan work types)
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

              (define (copy-input-files input inputs-directory)
                ;; Copy input files and update corresponding input objects.
                (cond
                 ((vector? input)
                  (vector-map copy-input-files
                              input))
                 ((eq? (object-type input)
                       'File)
                  (let ((path-in-inputs-directory
                         ;; Input files may have the same filename. So, we take
                         ;; the additional precaution of copying input files
                         ;; into their own hash-prefixed subdirectories, just
                         ;; like they are in the ravanan store.
                         (expand-file-name (file-name-join
                                            (take-right (file-name-split
                                                         (assoc-ref input "path"))
                                                        2))
                                           inputs-directory)))
                    (make-directories (file-dirname path-in-inputs-directory))
                    (copy-file (assoc-ref input "path")
                               path-in-inputs-directory)
                    (maybe-assoc-set input
                      (cons "location"
                            (just path-in-inputs-directory))
                      (cons "path"
                            (just path-in-inputs-directory))
                      (cons "basename"
                            (just (basename path-in-inputs-directory)))
                      (cons "nameroot"
                            (just (file-name-stem path-in-inputs-directory)))
                      (cons "nameext"
                            (just (file-name-extension path-in-inputs-directory)))
                      (cons "secondaryFiles"
                            (maybe-let* ((secondary-files
                                          (maybe-assoc-ref (just input) "secondaryFiles")))
                              (just (vector-map copy-input-files
                                                secondary-files)))))))
                 (else input)))

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

              (define (capture-secondary-file path secondary-file
                                              workflow-output-directory)
                "Capture @var{secondary-file} for primary @var{path} and return its
canonicalized value. If @var{required} is @code{#t} and no such secondary file
is found, error out. @var{workflow-output-directory} is path to the output
directory of the workflow."
                (let* ((secondary-file-path (secondary-path path secondary-file))
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

              (define (path+sha1->value path sha1 workflow-output-directory maybe-secondary-files)
                (maybe-assoc-set (copy-file-value (canonicalize-file-value
                                                   `(("class" . "File")
                                                     ("path" . ,path)
                                                     ("checksum" . ,(string-append "sha1$" sha1))))
                                                  workflow-output-directory)
                  (cons "secondaryFiles"
                        (maybe-let* ((secondary-files maybe-secondary-files))
                          (just (vector-filter-map (cut capture-secondary-file
                                                        path
                                                        <>
                                                        workflow-output-directory)
                                                   secondary-files))))))

              (define (path->value path workflow-output-directory maybe-secondary-files)
                (path+sha1->value path
                                  (sha1-hash path)
                                  workflow-output-directory
                                  maybe-secondary-files))

              (define (stdout-output->value workflow-output-directory
                                            stdout-directory
                                            stdout-filename
                                            output)
                (cons (assoc-ref output "id")
                      (let ((sha1 (sha1-hash stdout-filename)))
                        ;; Use path+sha1->value instead of path->value to avoid
                        ;; recomputing the SHA1 hash.
                        (path+sha1->value
                         (if (string=? stdout-filename
                                       (file-name-join* stdout-directory "stdout"))
                             ;; If stdout filename is unspecified, rename it to a
                             ;; hash of its contents.
                             (let ((hashed-filename
                                    (file-name-join* stdout-directory sha1)))
                               (rename-file stdout-filename
                                            hashed-filename)
                               hashed-filename)
                             ;; Else, return the stdout filename as it is.
                             stdout-filename)
                         sha1
                         workflow-output-directory
                         %nothing))))

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

              (define (stage-file file entry-name)
                ;; Stage file as entry-name and return the staged File value.
                (rename-file (assoc-ref* file "path")
                             entry-name)
                (canonicalize-file-value
                 (maybe-assoc-set `(("class" . "File")
                                    ("path" . ,entry-name))
                   (cons "secondaryFiles"
                         (maybe-let* ((secondary-files
                                       (maybe-assoc-ref (just file) "secondaryFiles")))
                           (just (vector-map (lambda (file)
                                               (stage-file file (assoc-ref* file "basename")))
                                             secondary-files)))))))

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
                                 (cons entry
                                       (stage-file entry entry-name))))))
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
                          ((name . value)
                           (setenv name value)))
                        '#$(match packages
                             ;; No package specifications; try the manifest
                             ;; file.
                             (#()
                              (manifest-file->environment manifest-file
                                                          inferior
                                                          guix-daemon-socket))
                             ;; Use package specifications to build an
                             ;; environment.
                             (_
                              (software-packages->environment packages
                                                              inferior
                                                              guix-daemon-socket))))

              (call-with-temporary-directory
               (lambda (inputs-directory)
                 (let ((inputs (map (match-lambda
                                      ((id . input)
                                       (cons id
                                             (copy-input-files input inputs-directory))))
                                    (json-string->scm
                                     (getenv "WORKFLOW_INPUTS"))))
                       (runtime `(("cores" . ,#$(cores batch-system)))))

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
    guix-store))
