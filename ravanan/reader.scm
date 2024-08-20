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

(define-module (ravanan reader)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (yaml)
  #:use-module (ravanan monads)
  #:use-module (ravanan vectors)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work utils)
  #:export (read-workflow
            read-inputs))

(define (preprocess-include tree)
  (cond
   ;; Arrays
   ((vector? tree)
    (vector-map preprocess-include tree))
   ;; Objects with an $include directive
   ((and (list? tree)
         (assoc-ref tree "$include"))
    ;; TODO: Warn if additional fields are present in object.
    => (lambda (file)
         ;; TODO: Support file, http and https URIs.
         (call-with-input-file file
           get-string-all)))
   ;; Other objects
   ((list? tree)
    (map (match-lambda
           ((key . value)
            (cons key
                  (preprocess-include value))))
         tree))
   ;; Atoms
   (else tree)))

(define* (coerce-alist->vector alist key-label #:optional value-label)
  "Convert @var{alist} to a vector of association lists by converting
keys in @var{alist} to values identified by @var{key-label} in each
association list of the returned vector of association lists. If
@var{value-label} is not @code{#f} and the values in @var{alist} are
not association lists themselves, map them to @var{value-label} in
each association list of the returned vector of association lists. If
@var{alist} is not an association list, but a vector, return it as is."
  (if (vector? alist)
      alist
      (map->vector (match-lambda
                     ((key value ...)
                      (acons key-label key
                             value))
                     ((key . value)
                      (if value-label
                          `((,key-label . ,key)
                            (,value-label . ,value))
                          (error "Unable to coerce pair into alist"
                                 (cons key value)))))
                   alist)))

(define (normalize-formals parameters)
  (vector-map (lambda (parameter)
                ;; Normalize optional and array types.
                (assoc-set parameter
                           (cons "type"
                                 (let ((type (assoc-ref parameter "type")))
                                   (cond
                                    ((not (string? type)) type)
                                    ((string-suffix? "?" type)
                                     (vector (substring type 0 (- (string-length type)
                                                                  (string-length "?")))
                                             'null))
                                    ((string-suffix? "[]" type)
                                     `(("type" . "array")
                                       ("items" . ,(substring type 0 (- (string-length type)
                                                                        (string-length "[]"))))))
                                    (else type))))))
              ;; Normalize formals to a vector.
              (coerce-alist->vector parameters "id" "type")))

(define (normalize-env-var-requirement env-var-requirement)
  (assoc-set env-var-requirement
             (cons "envDef"
                   (coerce-alist->vector
                    (assoc-ref env-var-requirement "envDef")
                    "envName" "envValue"))))

(define (normalize-requirements maybe-requirements)
  (maybe-let* ((requirements maybe-requirements))
    (just (vector-map (lambda (requirement)
                        (if (string=? (assoc-ref requirement "class")
                                      "EnvVarRequirement")
                            (normalize-env-var-requirement requirement)
                            requirement))
                      (coerce-alist->vector requirements "class")))))

(define (normalize-workflow cwl)
  "Normalize CWL workflow @var{cwl} (of any class)."
  (define (normalize-formal-input input)
    (if (eq? (formal-parameter-type (assoc-ref input "type"))
             'File)
        (maybe-assoc-set input
          (cons (list "default" "location")
                (maybe-let* ((location (maybe-assoc-ref (just input)
                                                        "default" "location")))
                  (just (canonicalize-path location)))))
        input))

  (define (normalize-base-command maybe-base-command)
    (maybe-let* ((base-command maybe-base-command))
      (cond
       ((string? base-command) (just (vector base-command)))
       ((vector? base-command) (just base-command)))))

  (define (normalize-arguments maybe-arguments)
    (maybe-let* ((arguments maybe-arguments))
      (just (vector-map (lambda (argument)
                          (cond
                           ((string? argument)
                            `(("valueFrom" . ,argument)))
                           ((list? argument)
                            argument)
                           (else
                            (error "Invalid argument" argument))))
                        arguments))))

  (define (normalize-steps maybe-steps)
    (maybe-let* ((steps maybe-steps))
      (just (vector-map (lambda (step)
                          (maybe-assoc-set step
                            ;; Read steps recursively.
                            (cons "run"
                                  (let ((run (assoc-ref step "run")))
                                    (just (if (string? run)
                                              (read-workflow run)
                                              (normalize-workflow run)))))
                            ;; Normalize step requirements and hints.
                            (cons "requirements"
                                  (normalize-requirements
                                   (maybe-assoc-ref (just step)
                                                    "requirements")))
                            (cons "hints"
                                  (normalize-requirements
                                   (maybe-assoc-ref (just step)
                                                    "hints")))))
                        (coerce-alist->vector steps "id")))))
  
  (apply maybe-assoc-set
         cwl
         ;; Normalize requirements and hints to a vector.
         (cons "requirements"
               (normalize-requirements
                (maybe-assoc-ref (just cwl) "requirements")))
         (cons "hints"
               (normalize-requirements
                (maybe-assoc-ref (just cwl) "hints")))
         ;; Normalize inputs, and resolve default locations of
         ;; File type inputs to their canonical paths.
         (cons "inputs"
               (just (vector-map normalize-formal-input
                                 (normalize-formals
                                  (assoc-ref cwl "inputs")))))
         ;; Normalize outputs.
         (cons "outputs"
               (just (normalize-formals (assoc-ref cwl "outputs"))))
         (let ((class (assoc-ref cwl "class")))
           (cond
            ((string=? class "CommandLineTool")
             ;; Normalize baseCommand to a vector, and arguments to
             ;; dictionaries.
             (list (cons "baseCommand"
                         (normalize-base-command
                          (maybe-assoc-ref (just cwl) "baseCommand")))
                   (cons "arguments"
                         (normalize-arguments
                          (maybe-assoc-ref (just cwl) "arguments")))))
            ((string=? class "Workflow")
             (list (cons "steps"
                         (normalize-steps
                          (maybe-assoc-ref (just cwl) "steps")))))
            (else
             (error "Unknown workflow class" class))))))

(define (read-workflow workflow-file)
  "Read CWL workflow (of any class) from @var{workflow-file}."
  (call-with-current-directory (dirname workflow-file)
    ;; TODO: Implement $import directive.
    (cut normalize-workflow
         (preprocess-include (read-yaml-file workflow-file)))))

(define (read-inputs inputs-file)
  "Read @var{inputs-file} resolving file paths if any."
  (call-with-current-directory (dirname inputs-file)
    (lambda ()
      (map (match-lambda
             ((input-id . input)
              (cons input-id
                    (let resolve-location ((input input))
                      (cond
                       ((vector? input)
                        (vector-map resolve-location
                                    input))
                       ((eq? (object-type input)
                             'File)
                        (assoc-set input
                                   (cons "location"
                                         (canonicalize-path
                                          (assoc-ref input "location")))))
                       (else input))))))
           ;; Even though YAML is a superset of JSON, use the JSON
           ;; reader if possible; it is simpler and less prone to type
           ;; ambiguities.
           (if (string=? (file-name-extension inputs-file)
                         ".json")
               (call-with-input-file inputs-file
                 json->scm)
               (read-yaml-file inputs-file))))))
