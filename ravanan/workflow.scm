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

(define-module (ravanan workflow)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module ((rnrs conditions) #:select (define-condition-type))
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (ravanan batch-system)
  #:use-module (ravanan command-line-tool)
  #:use-module (ravanan job-state)
  #:use-module (ravanan propnet)
  #:use-module (ravanan store)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work types)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (run-workflow))

(define %workflow-only-requirements
  (list "ScatterFeatureRequirement"
        "SubworkflowFeatureRequirement"))

(define %supported-requirements
  (append %workflow-only-requirements
          %command-line-tool-supported-requirements))

(define (supported-requirements batch-system)
  (append %workflow-only-requirements
          (command-line-tool-supported-requirements batch-system)))

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

(define-immutable-record-type <command-line-tool-state>
  (command-line-tool-state job-state formal-outputs)
  command-line-tool-state?
  (job-state command-line-tool-state-job-state)
  (formal-outputs command-line-tool-state-formal-outputs))

(define-immutable-record-type <workflow-state>
  (workflow-state propnet-state formal-outputs)
  workflow-state?
  (propnet-state workflow-state-propnet-state
                 set-workflow-state-propnet-state)
  (formal-outputs workflow-state-formal-outputs))

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

(define (maybe-list? maybe-value)
  "Return @code{#t} if maybe-monadic @var{maybe-list} is a vector. Else, return
@code{#f}."
  (from-maybe (maybe-let* ((value maybe-value))
                (just (list? value)))
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
   ;; If the values are lists, merge them element-wise.
   ((and (maybe-list? maybe-old-value)
         (maybe-list? maybe-new-value))
    (maybe-let* ((old-value maybe-old-value)
                 (new-value maybe-new-value))
      (just (map merge-values old-value new-value))))
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
                      (list (subset-requirements parent-requirements)
                            (subset-requirements step-requirements)))))
    (cons "hints"
          (just (fold inherit-requirements
                      (or (assoc-ref workflow "hints")
                          #())
                      (list (subset-requirements parent-hints)
                            (subset-requirements step-hints)))))))

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

(define* (workflow-class->propnet name cwl scheduler batch-system)
  "Return a propagator network scheduled using @var{scheduler} on
@var{batch-system} for @var{cwl}, a @code{Workflow} class workflow with
@var{name}."
  (define (normalize-scatter-method scatter-method)
    (assoc-ref* '(("dotproduct" . dot-product)
                  ("nested_crossproduct" . nested-cross-product)
                  ("flat_crossproduct" . flat-cross-product))
                scatter-method))

  (define (step->propagator step)
    (let* ((step-id (assoc-ref* step "id"))
           (step-propagator
            (command-line-tool->propagator step-id (assoc-ref* step "run"))))
      (propagator (propagator-name step-propagator)
                  (let ((proc (propagator-proc step-propagator)))
                    (scheduler-proc (scheduler-proc-name proc)
                                    (inherit-requirements-and-hints
                                     (scheduler-proc-cwl proc)
                                     (or (assoc-ref cwl "requirements")
                                         #())
                                     (or (assoc-ref cwl "hints")
                                         #())
                                     (or (assoc-ref step "requirements")
                                         #())
                                     (or (assoc-ref step "hints")
                                         #()))
                                    (maybe-assoc-ref (just step) "scatter")
                                    (maybe-bind (maybe-assoc-ref (just step) "scatterMethod")
                                                (compose just normalize-scatter-method))))
                  (map (match-lambda
                         ((input-id . _)
                          (cons input-id
                                (json-ref step "in" input-id))))
                       (propagator-inputs step-propagator))
                  (propagator-optional-inputs step-propagator)
                  (filter-map (match-lambda
                                ((output . cell)
                                 (and (vector-member output
                                                     (assoc-ref* step "out"))
                                      (cons output
                                            (string-append step-id "/" cell)))))
                              (propagator-outputs step-propagator)))))

  (maybe-let* ((requirements (maybe-assoc-ref (just cwl) "requirements")))
    (check-requirements requirements
                        batch-system
                        supported-requirements
                        %supported-requirements))
  (maybe-let* ((hints (maybe-assoc-ref (just cwl) "hints")))
    (check-requirements hints
                        batch-system
                        supported-requirements
                        %supported-requirements
                        #t))
  (propnet (vector-map->list step->propagator
                             (assoc-ref* cwl "steps"))
           value=?
           merge-values
           scheduler))

(define* (workflow-scheduler manifest-file channels scratch store batch-system
                             #:key guix-daemon-socket)
  (define (schedule proc inputs scheduler)
    "Schedule @var{proc} with inputs from the @var{inputs} association list. Return a
state-monadic job state object. @var{proc} may either be a @code{<propnet>}
object or a @code{<scheduler-proc>} object."
    (let* ((name (scheduler-proc-name proc))
           (cwl (scheduler-proc-cwl proc))
           (scatter (from-maybe (scheduler-proc-scatter proc)
                                #f))
           (scatter-method (from-maybe (scheduler-proc-scatter-method proc)
                                       #f))
           (class (assoc-ref* cwl "class")))
      (if scatter
          (case scatter-method
            ((dot-product)
             (apply state-map
                    (lambda input-elements
                      ;; Recurse with scattered inputs spliced in.
                      (schedule (scheduler-proc name cwl %nothing %nothing)
                                ;; Replace scattered inputs with single
                                ;; elements.
                                (apply assoc-set
                                       inputs
                                       (map cons
                                            (vector->list scatter)
                                            input-elements))
                                scheduler))
                    ;; Extract values of scattered inputs.
                    (vector-map->list (lambda (scatter-input)
                                        (vector->list (assoc-ref inputs scatter-input)))
                                      scatter)))
            ((nested-cross-product flat-cross-product)
             (error scatter-method
                    "Scatter method not implemented yet")))
          (let* ((formal-inputs (assoc-ref* cwl "inputs"))
                 ;; We need to resolve inputs after adding defaults since the
                 ;; default values may contain uninterned File objects.
                 (inputs (resolve-inputs (add-defaults inputs formal-inputs)
                                         formal-inputs
                                         store)))
            (cond
             ((string=? class "CommandLineTool")
              (state-let* ((job-state
                            (run-command-line-tool name
                                                   manifest-file
                                                   channels
                                                   cwl
                                                   inputs
                                                   scratch
                                                   store
                                                   batch-system
                                                   #:guix-daemon-socket guix-daemon-socket)))
                (state-return (command-line-tool-state job-state
                                                       (assoc-ref* cwl "outputs")))))
             ((string=? class "ExpressionTool")
              (error "Workflow class not implemented yet" class))
             ((string=? class "Workflow")
              (state-let* ((propnet-state
                            (schedule-propnet (workflow-class->propnet name
                                                                       cwl
                                                                       scheduler
                                                                       batch-system)
                                              inputs)))
                (state-return
                 (workflow-state propnet-state
                                 (assoc-ref* cwl "outputs"))))))))))

  (define (poll state)
    "Return updated state and current status of job @var{state} object as a
state-monadic @code{<state+status>} object. The status is one of the symbols
@code{pending} or @code{completed}."
    (cond
     ;; Return list states as completed only if all state elements in it are
     ;; completed.
     ((list? state)
      (state-let* ((polled-state+status (state-map poll state)))
        (state-return
         (state+status (map state+status-state
                            polled-state+status)
                       (if (every (lambda (state+status)
                                    (eq? (state+status-status state+status)
                                         'completed))
                                  polled-state+status)
                           'completed
                           'pending)))))
     ;; Poll job state. Raise an exception if the job has failed.
     ((command-line-tool-state? state)
      (let ((job-state (command-line-tool-state-job-state state)))
        (state-let* ((status (job-state-status job-state batch-system)))
          (state-return
           (state+status state
                         (case status
                           ((failed)
                            (raise-exception (job-failure
                                              (job-state-script job-state))))
                           (else => identity)))))))
     ;; Poll sub-workflow state. We do not need to check the status here since
     ;; job failures only occur at the level of a CommandLineTool.
     ((workflow-state? state)
      (state-let* ((updated-state+status
                    (poll-propnet (workflow-state-propnet-state state))))
        (state-return
         (state+status (set-workflow-state-propnet-state
                        state (state+status-state updated-state+status))
                       (state+status-status updated-state+status)))))
     (else
      (assertion-violation state "Invalid state"))))

  (define (find-output class outputs formal-output)
    "Find @var{formal-output} among @var{outputs}. @var{class} is the class of the
workflow."
    (let ((output-id (assoc-ref formal-output "id")))
      (cond
       ;; The output is present; cons and return it.
       ((cond
         ((string=? class "CommandLineTool")
          (assoc-ref outputs output-id))
         ((string=? class "ExpressionTool")
          (error "Workflow class not implemented yet"
                 class))
         ((string=? class "Workflow")
          (assoc-ref outputs
                     (assoc-ref* formal-output "outputSource"))))
        => (cut cons output-id <>))
       ;; The output is absent; check if a null type is acceptable.
       ((match-type 'null
                    (formal-parameter-type (assoc-ref* formal-output "type")))
        #f)
       ;; Else, error out.
       (else
        (error "output not found" output-id)))))

  (define (filter-outputs class outputs formal-outputs)
    "Filter @var{outputs} to only have outputs from @var{formal-outputs}. @var{class}
is the class of the workflow."
    (vector-filter-map->list (cut find-output class outputs <>)
                             formal-outputs))

  (define (capture-output state)
    "Return output of completed job @var{state}."
    (cond
     ((workflow-state? state)
      (state-let* ((outputs (capture-propnet-output
                             (workflow-state-propnet-state state))))
        (state-return
         (filter-outputs "Workflow"
                         outputs
                         (workflow-state-formal-outputs state)))))
     ((list? state)
      ;; Combine outputs from individual state elements.
      (state-let* ((captured-outputs (state-map capture-output state)))
        (match captured-outputs
          ((and (head-output _ ...)
                outputs)
           (state-return
            (map (match-lambda
                   ((id . value)
                    (cons id
                          (map->vector (lambda (output)
                                         ;; FIXME: Is this the correct way to
                                         ;; handle missing outputs?
                                         (or (assoc-ref output id)
                                             'null))
                                       outputs))))
                 head-output))))))
     (else
      ;; Log progress and return captured output.
      (let ((script (job-state-script (command-line-tool-state-job-state state))))
        (state-return
         (begin
           (format (current-error-port)
                   "~a completed; logs at ~a and ~a~%"
                   script
                   (script->store-stdout-file script store)
                   (script->store-stderr-file script store))
           (filter-outputs "CommandLineTool"
                           (capture-command-line-tool-output script store)
                           (command-line-tool-state-formal-outputs state))))))))

  (scheduler schedule poll capture-output))

(define (add-defaults inputs formal-inputs)
  "Add default values from @var{formal-inputs} to @var{inputs}."
  (vector-filter-map->list (lambda (formal-input)
                             (let* ((id (assoc-ref* formal-input "id"))
                                    ;; Try
                                    ;; - the input value
                                    ;; - the default value
                                    ;; - the null value (for optional inputs)
                                    (value (or (assoc-ref inputs id)
                                               (assoc-ref formal-input "default")
                                               'null))
                                    (expected-type (formal-parameter-type
                                                    (assoc-ref* formal-input "type"))))
                               (unless (match-type value expected-type)
                                 (user-error "Type mismatch for input `~a'; expected `~a' but got `~a'"
                                             id expected-type (object-type value)))
                               (and (not (eq? value 'null))
                                    (cons id value))))
                           formal-inputs))

(define (resolve-inputs inputs formal-inputs store)
  "Traverse @var{inputs} and @var{formal-inputs} recursively, intern any
files found into the @var{store} and return a tree of the fully resolved inputs."
  (define (match-secondary-file-pattern input pattern)
    "Return @code{#t} if secondary file @var{pattern} matches at least one secondary
file in @var{input}."
    ;; TODO: Implement caret characters in SecondaryFileSchema DSL.
    (vector-any (lambda (secondary-file)
                  (string=? (store-item-name (assoc-ref* secondary-file "path"))
                            (string-append (store-item-name (assoc-ref* input "path"))
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
                      (let ((resolved-input (intern-file input store)))
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

(define* (run-workflow name manifest-file channels cwl inputs
                       scratch store batch-system
                       #:key guix-daemon-socket)
  "Run a workflow @var{cwl} named @var{name} with @var{inputs} using
tools from Guix manifest in @var{manifest-file}. If @var{channels} is not
@code{#f}, build the manifest in a Guix inferior with @var{channels}.

@var{scratch} is the path to the scratch area on all worker nodes. The scratch
area need not be shared. @var{store} is the path to the shared ravanan store.
@var{batch-system} is an object representing one of the supported batch systems.

@var{guix-daemon-socket} is the Guix daemon socket to connect to."
  (guard (c ((job-failure? c)
             (let ((script (job-failure-script c)))
               (user-error
                "~a failed; logs at ~a and ~a~%"
                script
                (script->store-stdout-file script store)
                (script->store-stderr-file script store)))))
    (let ((scheduler (workflow-scheduler
                      manifest-file channels scratch store batch-system
                      #:guix-daemon-socket guix-daemon-socket)))
      (run-with-state
       (let loop ((mstate ((scheduler-schedule scheduler)
                           (scheduler-proc name cwl %nothing %nothing)
                           inputs
                           scheduler)))
         ;; Poll.
         (state-let* ((state mstate)
                      (state+status ((scheduler-poll scheduler) state)))
           (if (eq? (state+status-status state+status)
                    'pending)
               (begin
                 ;; Pause before looping and polling again so we don't bother the
                 ;; job server too often.
                 (sleep (cond
                         ;; Single machine jobs are run synchronously. So, there
                         ;; is no need to wait to poll them.
                         ((eq? batch-system 'single-machine)
                          0)
                         ((slurm-api-batch-system? batch-system)
                          %job-poll-interval)))
                 (loop (state-return (state+status-state state+status))))
               ;; Capture outputs.
               ((scheduler-capture-output scheduler)
                (state+status-state state+status)))))))))
