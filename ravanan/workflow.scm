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

(define-module (ravanan workflow)
  #:use-module ((rnrs conditions) #:select (define-condition-type))
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (ravanan command-line-tool)
  #:use-module (ravanan job-state)
  #:use-module (ravanan propnet)
  #:use-module (ravanan reader)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work types)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (run-workflow))

(define %supported-requirements
  (cons* "ScatterFeatureRequirement"
         "SubworkflowFeatureRequirement"
         %command-line-tool-supported-requirements))

;; In batch systems that require it, poll job completion status every
;; 5 seconds.
(define %job-poll-interval
  5)

(define-condition-type &job-failure &error
  job-failure job-failure?
  (script job-failure-script))

(define-immutable-record-type <scheduler-proc>
  (scheduler-proc name cwl scatter scatter-method)
  scheduler-proc?
  (name scheduler-proc-name)
  (cwl scheduler-proc-cwl)
  (scatter scheduler-proc-scatter)
  (scatter-method scheduler-proc-scatter-method))

(define (value=? maybe-val1 maybe-val2)
  "Return @code{#t} if maybe-monadic values @var{maybe-val1} and
@var{maybe-val2} are the same value. Else, return @code{#f}."
  ;; TODO: Support more types.
  (or (and (nothing? maybe-val1)
           (nothing? maybe-val2))
      (from-maybe (maybe-let* ((val1 maybe-val1)
                               (val2 maybe-val2))
                    (just (equal? val1 val2)))
                  #f)))

(define (maybe-vector? maybe-value)
  "Return @code{#t} if maybe-monadic @var{maybe-value} contains a vector. Else,
return @code{#f}."
  (from-maybe (maybe-let* ((value maybe-value))
                (just (vector? value)))
              #f))

(define (merge-values maybe-old-value maybe-new-value)
  "Merge values @var{maybe-old-value} and @var{maybe-new-value} into a
single value."
  (cond
   ;; If only one of the two values is Nothing, pick the one that
   ;; isn't.
   ((and (nothing? maybe-old-value)
         (not (nothing? maybe-new-value)))
    maybe-new-value)
   ((and (not (nothing? maybe-old-value))
         (nothing? maybe-new-value))
    maybe-old-value)
   ;; If the values are vectors, merge them element-wise.
   ((and (maybe-vector? maybe-old-value)
         (maybe-vector? maybe-new-value))
    (maybe-let* ((old-value maybe-old-value)
                 (new-value maybe-new-value))
      (just (vector-map merge-values old-value new-value))))
   (else
    (if (value=? maybe-old-value maybe-new-value)
        ;; If the values are equal, pick one arbitrarily.
        maybe-old-value
        ;; If the values are not equal, raise a contradiction.
        (error "Contradiction")))))

(define (inherit-requirements-and-hints workflow
                                        parent-requirements
                                        parent-hints
                                        step-requirements
                                        step-hints)
  "Return an updated @var{workflow} with requirements and hints inherited
from its parent and its step. @var{parent-requirements} and
@var{parent-hints} are the requirements and hints of the
parent. @var{step-requirements} and @var{step-hints} are the
requirements and hints of the step."
  (define (subset-requirements requirements)
    (if (string=? (assoc-ref workflow "class")
                  "CommandLineTool")
        (vector-filter (lambda (requirement)
                         (member (assoc-ref requirement "class")
                                 %command-line-tool-supported-requirements))
                       requirements)
        requirements))
  
  (maybe-assoc-set workflow
    (cons "requirements"
          (just (fold inherit-requirements
                      (or (assoc-ref workflow "requirements")
                          #())
                      (subset-requirements parent-requirements)
                      (subset-requirements step-requirements))))
    (cons "hints"
          (just (fold inherit-requirements
                      (or (assoc-ref workflow "hints")
                          #())
                      (subset-requirements parent-hints)
                      (subset-requirements step-hints))))))

(define (coerce-type val type)
  "Coerce @var{val} to @var{type}."
  ;; This function exists to handle YAML's type ambiguities.
  (case type
    ((boolean)
     (cond
      ((member val (list "true" "yes")) #t)
      ((member val (list "false" "no")) #f)
      (else (error "Unable to coerce value to type" val type))))
    (else val)))

(define (optional-input? input)
  "Return @code{#t} if @var{input} is optional. Else, return @code{#f}."
  ;; Inputs that either have a default or accept null values are optional.
  (and (or (assoc-ref input "default")
           (match-type 'null
                       (formal-parameter-type
                        (assoc-ref* input "type"))))
       (assoc-ref input "id")))

(define* (command-line-tool->propagator name cwl)
  "Convert @code{CommandLineTool} workflow @var{cwl} of @var{name} to a
propagator."
  (propagator name
              (scheduler-proc name cwl %nothing %nothing)
              (vector-map->list (lambda (input)
                                  (cons (assoc-ref input "id")
                                        (assoc-ref input "id")))
                                (assoc-ref cwl "inputs"))
              ;; Inputs that either have a default or accept null values are
              ;; optional.
              (vector-filter-map->list optional-input?
                                       (assoc-ref cwl "inputs"))
              (vector-map->list (lambda (output)
                                  (cons (assoc-ref output "id")
                                        (assoc-ref output "id")))
                                (assoc-ref cwl "outputs"))))

(define (workflow->command-line-tool-steps cwl)
  "Recursively traverse @var{cwl} tree and return a list of
@code{CommandLineTool} class steps."
  (define (inherit-defaults step-run step-in)
    "Augment the @var{step-run} CWL tree with defaults from the containing
workflow. @var{step-in} is the inputs mapping association list of the step."
    (assoc-set step-run
      (cons "inputs"
            (vector-map (lambda (input)
                          (let* ((input-id (assoc-ref input "id"))
                                 (input-source (assoc-ref step-in input-id)))
                            (maybe-assoc-set input
                              (cons "default"
                                    (maybe-assoc-ref (just cwl)
                                                     "inputs"
                                                     input-source
                                                     "default")))))
                        (assoc-ref* step-run "inputs")))))

  (vector-append-map->list (lambda (step)
                             (let ((run (inherit-defaults (assoc-ref step "run")
                                                          (assoc-ref step "in"))))
                               (if (string=? (assoc-ref run "class")
                                             "CommandLineTool")
                                   (list (assoc-set step
                                           (cons "run" run)))
                                   (workflow->command-line-tool-steps
                                    (inherit-requirements-and-hints
                                     run
                                     (or (assoc-ref cwl "requirements")
                                         #())
                                     (or (assoc-ref cwl "hints")
                                         #())
                                     (or (assoc-ref run "requirements")
                                         #())
                                     (or (assoc-ref run "hints")
                                         #()))))))
                           (assoc-ref cwl "steps")))

(define* (workflow-class->propagators name cwl)
  "Return a list of propagators for @var{cwl}, a @code{Workflow} class
workflow with @var{name}."
  (define (prefix-name prefix name)
    "Prefix @var{name} with @var{prefix} so as to keep subworkflows in
their own namespaces."
    (string-append prefix "/" name))

  (map (lambda (step)
         (let* ((step-id (assoc-ref step "id"))
                (step-propagator
                 (command-line-tool->propagator step-id
                                                (assoc-ref step "run"))))
           (propagator (prefix-name step-id
                                    (propagator-name step-propagator))
                       ;; Augment proc with scatter and scatter method.
                       (let ((proc (propagator-proc step-propagator)))
                         (scheduler-proc
                          (scheduler-proc-name proc)
                          (scheduler-proc-cwl proc)
                          (maybe-assoc-ref (just step) "scatter")
                          (maybe-let* ((scatter-method
                                        (maybe-assoc-ref (just step) "scatterMethod")))
                            (just (assoc-ref* '(("dotproduct" . dot-product)
                                                ("nested_crossproduct" . nested-cross-product)
                                                ("flat_crossproduct" . flat-cross-product))
                                              scatter-method)))))
                       (assoc-ref step "in")
                       (propagator-optional-inputs step-propagator)
                       (vector-map->list (lambda (output-name)
                                           (cons output-name
                                                 (prefix-name step-id output-name)))
                                         (assoc-ref step "out")))))
       (workflow->command-line-tool-steps cwl)))

(define (workflow->propagators name cwl)
  "Return a list of propagators for @var{cwl}, a workflow with
@var{name}."
  (let ((class (assoc-ref cwl "class")))
    ((cond
      ((string=? class "CommandLineTool")
       (compose list command-line-tool->propagator))
      ((string=? class "ExpressionTool")
       (error "Workflow class not implemented yet" class))
      ((string=? class "Workflow")
       (maybe-let* ((requirements (maybe-assoc-ref (just cwl) "requirements")))
         (check-requirements requirements %supported-requirements))
       (maybe-let* ((hints (maybe-assoc-ref (just cwl) "hints")))
         (check-requirements hints %supported-requirements #t))
       workflow-class->propagators)
      (else
       (error "Invalid workflow class" class)))
     name cwl)))

(define* (workflow-scheduler manifest scratch store batch-system
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
                      (schedule (scheduler-proc name cwl %nothing %nothing)
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
             (error scatter-method
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
    "Return current status and updated state of job @var{state} object. The status is
one of the symbols @code{pending} or @code{completed}. Raise an exception and
exit if job has failed."
    (guard (c ((job-failure? c)
               (let ((script (job-failure-script c)))
                 (user-error
                  "~a failed; logs at ~a and ~a~%"
                  script
                  (script->store-stdout-file script store)
                  (script->store-stderr-file script store)))))
      (let ((status state (job-state-status state
                                            #:slurm-api-endpoint slurm-api-endpoint
                                            #:slurm-jwt slurm-jwt)))
        (values (case status
                  ((failed)
                   (raise-exception (job-failure (job-state-script state))))
                  (else => identity))
                state))))

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
        (let ((script (job-state-script state)))
          (format (current-error-port)
                  "~a completed; logs at ~a and ~a~%"
                  script
                  (script->store-stdout-file script store)
                  (script->store-stderr-file script store))
          (capture-command-line-tool-output script store))))

  (scheduler schedule poll capture-output))

(define (location->path location)
  "Convert file @var{location} URI to path."
  (if (string-prefix? "/" location)
      ;; Sometimes location is actually a path. In that case, return as is.
      location
      ;; If location is an URI, parse the URI and return the path part.
      (uri-path (string->uri location))))

(define (intern-file file store)
  "Intern @var{file} into the ravanan @var{store} unless it is already a store
path. Return the interned path."
  (if (string-prefix? store file)
      ;; If file is already a store path, return it as is.
      file
      ;; Else, intern it and return the interned path.
      (let ((interned-path
             (expand-file-name
              (file-name-join* %store-files-directory
                               (string-append (sha1-hash file)
                                              "-"
                                              (basename file)))
              store)))
        (copy-file file interned-path)
        interned-path)))

(define (resolve-inputs inputs formal-inputs store)
  "Traverse @var{inputs} and @var{formal-inputs} recursively, intern any
files found into the @var{store} and return a tree of the fully resolved inputs.

The returned @code{File} type objects are updated with @code{basename},
@code{nameroot}, @code{nameext}, @code{checksum} and @code{size} fields, and
store-interned paths in the @code{location} and @code{path} fields. The
@code{basename} field contains the basename of the original path, and not the
store-interned path."
  (define (canonicalize-file-input input)
    "Canonicalize @code{File} type @var{input} and its secondary files."
    (let* ((path (or (and (assoc-ref input "location")
                          (location->path (assoc-ref input "location")))
                     (assoc-ref input "path")))
           (interned-path (intern-file path store)))
      (maybe-assoc-set input
        (cons "location" (just interned-path))
        (cons "path" (just interned-path))
        (cons "basename" (just (basename path)))
        (cons "nameroot" (just (file-name-stem path)))
        (cons "nameext" (just (file-name-extension path)))
        (cons "checksum" (just (checksum path)))
        (cons "size" (just (stat:size (stat path))))
        (cons "secondaryFiles"
              (maybe-let* ((secondary-files (maybe-assoc-ref (just input)
                                                             "secondaryFiles")))
                (just (vector-map canonicalize-file-input
                                  secondary-files)))))))

  (define (match-secondary-file-pattern input pattern)
    "Return @code{#t} if secondary file @var{pattern} matches at least one secondary
file in @var{input}."
    ;; TODO: Implement caret characters in SecondaryFileSchema DSL.
    (vector-any (lambda (secondary-file)
                  (string=? (assoc-ref* secondary-file "path")
                            (string-append (assoc-ref* input "path")
                                           pattern)))
                (or (assoc-ref input "secondaryFiles")
                    (user-error "Missing secondaryFiles in input ~a"
                                input))))

  (define (check-secondary-files input secondary-files)
    "Check if all required @var{secondary-files} are present in @var{input}. If not,
error out."
    (vector-for-each (lambda (secondary-file)
                       (let ((pattern (assoc-ref* secondary-file "pattern")))
                         (when (and (assoc-ref* secondary-file "required")
                                    (not (match-secondary-file-pattern input pattern)))
                           (user-error "Secondary file ~a missing in input ~a"
                                       pattern
                                       input))))
                     secondary-files))

  (define (resolve inputs types maybe-secondary-files)
    (vector-map (lambda (input type-tree maybe-secondary-files)
                  ;; Check type.
                  (let* ((type (formal-parameter-type type-tree))
                         (matched-type (match-type input type)))
                    (unless matched-type
                      (error input "Type mismatch" input type))
                    ;; TODO: Implement record and enum types.
                    (cond
                     ;; Recurse over array types.
                     ((array-type? matched-type)
                      (resolve input
                               (make-vector (vector-length input)
                                            (assoc-ref type-tree "items"))
                               (make-vector (vector-length input)
                                            maybe-secondary-files)))
                     ;; Intern File type inputs and fully resolve them.
                     ((eq? matched-type 'File)
                      (let ((resolved-input (canonicalize-file-input input)))
                        ;; Ensure secondary files are provided with File type
                        ;; inputs.
                        (maybe-bind maybe-secondary-files
                                    (cut check-secondary-files resolved-input <>))
                        resolved-input))
                     ;; Return other input types unchanged.
                     (else input))))
                inputs
                types
                maybe-secondary-files))

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
                                                 (user-error "Type of input ~a not specified"
                                                             id))))
                                         formal-inputs)
                             (vector-map (lambda (formal-input)
                                           (maybe-assoc-ref (just formal-input)
                                                            "secondaryFiles"))
                                         formal-inputs))
                    formal-inputs))

(define* (run-workflow name manifest cwl inputs
                       scratch store batch-system
                       #:key guix-daemon-socket
                       slurm-api-endpoint slurm-jwt)
  "Run a workflow @var{cwl} named @var{name} with @var{inputs} using
tools from Guix @var{manifest}.

@var{scratch} is the path to the scratch area on all worker nodes. The
scratch area need not be shared. @var{store} is the path to the shared
ravanan store. @var{batch-system} is a symbol representing one of the
supported batch systems (either @code{'single-machine} or
@code{'slurm-api}).

@var{guix-daemon-socket} is the Guix daemon socket to connect to.

@var{slurm-api-endpoint}, a @code{<uri>} object, is the slurm API
endpoint to connect to. @var{slurm-jwt}, a string, is the JWT token to
authenticate to the slurm API with. @var{slurm-api-endpoint} and
@var{slurm-jwt} are only used when @var{batch-system} is
@code{'slurm-api}."
  (define (capture-output cell-values output)
    (let ((output-id (assoc-ref output "id")))
      (cond
       ;; The output is present; cons and return it.
       ((let ((class (assoc-ref* cwl "class")))
          (cond
           ((string=? class "CommandLineTool")
            (assoc-ref cell-values output-id))
           ((string=? class "ExpressionTool")
            (error "Workflow class not implemented yet"
                   class))
           ((string=? class "Workflow")
            (assoc-ref cell-values
                       (assoc-ref* output "outputSource")))))
        => (cut cons output-id <>))
       ;; The output is absent; check if a null type is acceptable.
       ((match-type 'null
                    (formal-parameter-type (assoc-ref* output "type")))
        #f)
       ;; Else, error out.
       (else
        (error "output not found" output-id)))))

  (let ((inputs (resolve-inputs inputs (assoc-ref* cwl "inputs") store)))
    ;; Ensure required inputs are specified.
    (vector-for-each (lambda (input)
                       (let ((input-id (assoc-ref input "id")))
                         (unless (or (optional-input? input)
                                     (assoc input-id inputs))
                           (user-error "Required input `~a' not specified"
                                       input-id))))
                     (assoc-ref cwl "inputs"))
    (let loop ((state (schedule-propnet
                       (propnet (workflow->propagators name cwl)
                                value=?
                                merge-values
                                (workflow-scheduler
                                 manifest scratch store batch-system
                                 #:guix-daemon-socket guix-daemon-socket
                                 #:slurm-api-endpoint slurm-api-endpoint
                                 #:slurm-jwt slurm-jwt))
                       inputs)))
      ;; Poll.
      (let ((status state (poll-propnet state)))
        (if (eq? status 'pending)
            (begin
              ;; Pause before looping and polling again so we don't bother the job
              ;; server too often.
              (sleep (case batch-system
                       ;; Single machine jobs are run synchronously. So, there is
                       ;; no need to wait to poll them.
                       ((single-machine) 0)
                       ((slurm-api) %job-poll-interval)))
              (loop state))
            ;; Capture outputs.
            (vector-filter-map->list (cute capture-output
                                           (capture-propnet-output state)
                                           <>)
                                     (assoc-ref* cwl "outputs")))))))
