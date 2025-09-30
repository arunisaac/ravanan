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

(define-module (ravanan reader)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module ((yaml) #:prefix yaml:)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work types)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (read-workflow
            read-inputs
            coerce-type))

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
  (define (normalize-type type)
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
     (else type)))

  (vector-map (lambda (parameter)
                ;; Normalize optional and array types.
                (assoc-set parameter
                  (cons "type"
                        (normalize-type (assoc-ref parameter "type")))))
              ;; Normalize formals to a vector.
              (coerce-alist->vector parameters "id" "type")))

(define (normalize-env-var-requirement env-var-requirement)
  (assoc-set env-var-requirement
    (cons "envDef"
          (coerce-alist->vector
           (assoc-ref env-var-requirement "envDef")
           "envName" "envValue"))))

(define (normalize-software-requirement software-requirement)
  (maybe-assoc-set software-requirement
    ;; Canonicalize manifest file path so that we look it up with respect to the
    ;; path of the workflow file.
    (cons "manifest"
          (maybe-bind (maybe-assoc-ref (just software-requirement)
                                       "manifest")
                      (compose just canonicalize-path)))))

(define (normalize-requirements maybe-requirements)
  (maybe-let* ((requirements maybe-requirements))
    (just (vector-map (lambda (requirement)
                        (let ((class (assoc-ref requirement "class")))
                          (cond
                           ((string=? class "EnvVarRequirement")
                            (normalize-env-var-requirement requirement))
                           ((string=? class "SoftwareRequirement")
                            (normalize-software-requirement requirement))
                           (else
                            requirement))))
                      (coerce-alist->vector requirements "class")))))

(define (normalize-secondary-files secondary-files default-required)
  "Normalize @var{secondary-files}. @var{default-required} is the default value of
the @code{required} field when it is not specified."
  (cond
   ;; array of SecondaryFileSchema objects
   ((vector? secondary-files)
    (vector-append-map (cut normalize-secondary-files <> default-required)
                       secondary-files))
   ;; SecondaryFileSchema object
   ((not (string? secondary-files))
    (vector secondary-files))
   ;; string form optional SecondaryFileSchema object
   ((string-suffix? "?" secondary-files)
    (vector `(("pattern" . ,(string-drop-right secondary-files
                                               (string-length "?")))
              ("required" . #f))))
   ;; string form SecondaryFileSchema object with an unspecified required
   (else
    (vector `(("pattern" . ,secondary-files)
              ("required" . ,default-required))))))

(define (some-file-type? type)
  "Return @code{#t} if @var{type} is a @code{File}, an array of @code{File}s, an
array of array of @code{File}s, etc. Else, return @code{#f}"
  (or (eq? type 'File)
      (and (cwl-array-type? type)
           (some-file-type? (cwl-array-type-subtype type)))))

(define (normalize-formal-input input)
  "Normalize formal @var{input}."
  (if (some-file-type? (formal-parameter-type (assoc-ref input "type")))
      (maybe-assoc-set input
        (cons "default"
              (maybe-bind (maybe-assoc-ref (just input) "default")
                          (compose just normalize-input)))
        (cons "secondaryFiles"
              (maybe-bind (maybe-assoc-ref (just input) "secondaryFiles")
                          (compose just
                                   (cut normalize-secondary-files <> #t)))))
      input))

(define (normalize-formal-output output)
  "Normalize formal @var{output}."
  (if (some-file-type? (formal-parameter-type (assoc-ref output "type")))
      (maybe-assoc-set output
        (cons "secondaryFiles"
              (maybe-bind (maybe-assoc-ref (just output) "secondaryFiles")
                          (compose just
                                   (cut normalize-secondary-files <> #f)))))
      output))

(define (normalize-base-command maybe-base-command)
  "Normalize @var{base-command} of @code{CommandLineTool} class workflow."
  (maybe-let* ((base-command maybe-base-command))
    (cond
     ((string? base-command) (just (vector base-command)))
     ((vector? base-command) (just base-command)))))

(define (normalize-arguments maybe-arguments)
  "Normalize @var{maybe-arguments} of @code{CommandLineTool} class workflow."
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
  "Normalize @var{maybe-steps} of @code{Workflow} class workflow."
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

(define (normalize-workflow cwl)
  "Normalize CWL workflow @var{cwl} (of any class)."
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
               (just (vector-map normalize-formal-output
                                 (normalize-formals
                                  (assoc-ref cwl "outputs")))))
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
  (guard (c ((and (who-condition? c)
                  (eq? (condition-who c)
                       'read-yaml-file)
                  (irritants-condition? c))
             (user-error "Unable to read workflow file ~a: ~a"
                         workflow-file
                         (string-join (condition-irritants c)
                                      ": "))))
    (let ((workflow-path (guard (c ((unsupported-uri-scheme? c)
                                    (user-error "Unsupported URI scheme ~a in ~a"
                                                (unsupported-uri-scheme-scheme c)
                                                workflow-file)))
                           (location->path workflow-file))))
      (call-with-current-directory (dirname workflow-path)
        ;; TODO: Implement $import directive.
        (cut normalize-workflow
             (preprocess-include (read-yaml-file (basename workflow-path))))))))

(define (normalize-input input)
  "Normalize actual @var{input}."
  (cond
   ((vector? input)
    (vector-map normalize-input
                input))
   ((eq? (object-type input)
         'File)
    (canonicalize-file-value input))
   (else input)))

(define (read-inputs inputs-file)
  "Read @var{inputs-file} resolving file paths if any."
  (guard (c ((and (who-condition? c)
                  (memq (condition-who c)
                        '(read-json-file read-yaml-file))
                  (irritants-condition? c))
             (user-error "Unable to read inputs file ~a: ~a"
                         inputs-file
                         (string-join (condition-irritants c)
                                      ": "))))
    (let ((inputs-path (guard (c ((unsupported-uri-scheme? c)
                                  (user-error "Unsupported URI scheme ~a in ~a"
                                                (unsupported-uri-scheme-scheme c)
                                                inputs-file)))
                         (location->path inputs-file))))
      (call-with-current-directory (dirname inputs-path)
        (lambda ()
          (map (match-lambda
                 ((input-id . input)
                  (cons input-id
                        (normalize-input input))))
               ;; Even though YAML is a superset of JSON, use the JSON
               ;; reader if possible; it is simpler and less prone to type
               ;; ambiguities.
               (if (string=? (file-name-extension inputs-path)
                             ".json")
                   (read-json-file (basename inputs-path))
                   (read-yaml-file (basename inputs-path)))))))))

(define (coerce-type val type)
  "Coerce @var{val} to @var{type}."
  ;; This function exists to handle YAML's type ambiguities.
  (case type
    ((boolean)
     (cond
      ((member val (list "true" "yes")) #t)
      ((member val (list "false" "no")) #f)
      (else (error "Unable to coerce value to type" val type))))
    ((number)
     (if (number? val)
         val
         (string->number val)))
    (else val)))

(define (read-json-file file)
  "Read JSON @var{file} and return scheme tree."
  (guard (c (else
             (raise-exception
              (condition (make-who-condition 'read-json-file)
                         c))))
    (call-with-input-file file
      json->scm)))

(define (read-yaml-file file)
  "Read YAML @var{file} and return scheme tree."
  (guard (c ((and (message-condition? c)
                  (string-prefix? "read-yaml-file:" (condition-message c)))
             (raise-exception
              (condition (make-who-condition 'read-yaml-file)
                         (make-irritants-condition
                          (match (string-split (condition-message c) #\:)
                            ((_ message file)
                             (list (string-trim message)
                                   (string-trim file)))))))))
    (yaml:read-yaml-file file)))
