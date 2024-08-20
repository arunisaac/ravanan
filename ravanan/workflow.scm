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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (ravanan command-line-tool)
  #:use-module (ravanan monads)
  #:use-module (ravanan propnet)
  #:use-module (ravanan reader)
  #:use-module (ravanan utils)
  #:use-module (ravanan vectors)
  #:use-module (ravanan work command-line-tool)
  #:export (run-workflow))

(define %supported-requirements
  (cons "SubworkflowFeatureRequirement"
        %command-line-tool-supported-requirements))

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

(define* (command-line-tool->propagator name cwl)
  "Convert @code{CommandLineTool} workflow @var{cwl} of @var{name} to a
propagator."
  (propagator name
              (list name cwl)
              (vector-map->list (lambda (input)
                                  (cons (assoc-ref input "id")
                                        (assoc-ref input "id")))
                                (assoc-ref cwl "inputs"))
              (vector-map->list (lambda (output)
                                  (cons (assoc-ref output "id")
                                        (assoc-ref output "id")))
                                (assoc-ref cwl "outputs"))))

(define (workflow->command-line-tool-steps cwl)
  "Recursively traverse @var{cwl} tree and return a list of
@code{CommandLineTool} class steps."
  (vector-append-map->list (lambda (step)
                             (let ((run (assoc-ref step "run")))
                               (if (string=? (assoc-ref run "class")
                                             "CommandLineTool")
                                   (list step)
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
                       (propagator-proc step-propagator)
                       (assoc-ref step "in")
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
       workflow-class->propagators)
      (else
       (error "Invalid workflow class" class)))
     name cwl)))

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
  (let ((cell-values
         (run-propnet
          (propnet (workflow->propagators name cwl)
                   value=?
                   merge-values
                   (command-line-tool-scheduler
                    manifest scratch store batch-system
                    #:guix-daemon-socket guix-daemon-socket
                    #:slurm-api-endpoint slurm-api-endpoint
                    #:slurm-jwt slurm-jwt))
          inputs)))
    (vector-map->list (lambda (output)
                        (cons (assoc-ref output "id")
                              (assoc-ref cell-values
                                         (assoc-ref output "outputSource"))))
                      (assoc-ref cwl "outputs"))))
