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

(define-module (ravanan work command-line-tool)
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (json)
  #:use-module (ravanan work types)
  #:use-module (ravanan work utils)
  #:export (value->string
            call-with-current-directory
            object-type
            match-type
            glob-match-type
            formal-parameter-type
            run-command
            sha1-hash
            checksum
            evaluate-javascript
            evaluate-parameter-reference))

(define (value->string x)
  "Convert value @var{x} to a string."
  (cond
   ((number? x) (number->string x))
   (else x)))

(define (call-with-current-directory curdir thunk)
  "Call THUNK with current directory set to CURDIR. Restore current
directory after THUNK returns."
  (let ((original-current-directory (getcwd)))
    (dynamic-wind (cut chdir curdir)
                  thunk
                  (cut chdir original-current-directory))))

;; TODO: Support float types.
(define (object-type obj)
  "Return the type of @var{obj}."
  (cond
   ((eq? obj 'null) 'null)
   ((boolean? obj) 'boolean)
   ((integer? obj) 'int)
   ((string? obj) 'string)
   ;; For vector objects, we use the first element to guess at the
   ;; subtype.
   ((vector? obj)
    (match obj
      (#(head _ ...)
       (array-type (object-type head)))
      (#()
       (array-type 'Any))))
   ;; File and Directory objects
   ((assoc-ref obj "class") => string->symbol)
   (else
    (error "Unable to determine type of object" obj))))

(define (match-type obj type)
  "Match the type of @var{obj} to @var{type} and return the matching
type. The returned type may be different from @var{type}---for
example, when @var{type} is a union type."
  (cond
   ;; Match any type.
   ((eq? type 'Any)
    (object-type obj))
   ;; Match null values or empty arrays.
   ((eq? type 'null)
    (match obj
      ((or 'null #()) 'null)
      (_ #f)))
   ;; Recursively match type of every element of array.
   ((array-type? type)
    (and (vector? obj)
         (every (cut match-type <> (array-type-subtype type))
                (vector->list obj))
         type))
   ;; Match any one of the subtypes of the union type.
   ((union-type? type)
    (any (cut match-type obj <>)
         (union-type-subtypes type)))
   ;; Else, match the exact type of the object.
   (else
    (and (eq? (object-type obj)
              type)
         type))))

;; glob results need a special type matching because a singleton array
;; of files can match a scalar File type.
(define (glob-match-type matches type)
  "Match list of glob @var{matches} to @var{type} and return the matching
type. The returned type may be different from @var{type}---for
example, when @var{type} is a union type."
  (cond
   ((eq? type 'null)
    (match matches
      (() type)
      (_ #f)))
   ((eq? type 'File)
    (match matches
      ((single-match)
       (and (file-exists? single-match)
            (eq? (stat:type (stat single-match))
                 'regular)
            type))
      (_ #f)))
   ((eq? type 'Directory)
    (match matches
      ((single-match)
       (and (file-exists? single-match)
            (eq? (stat:type (stat single-match))
                 'directory)
            type))
      (_ #f)))
   ((array-type? type)
    (and (every (lambda (m)
                  (glob-match-type (list m)
                                   (array-type-subtype type)))
                matches)
         type))
   ((union-type? type)
    (any (cut glob-match-type matches <>)
         (union-type-subtypes type)))))

;; TODO: Support all types.
(define (formal-parameter-type type)
  "Return the type described in the CWL @var{type} specification."
  (cond
   ;; Union types
   ((vector? type)
    (apply union-type
           (map formal-parameter-type (vector->list type))))
   ;; Other types
   ((string? type)
    (string->symbol type))
   ;; Array types
   ((string=? (assoc-ref type "type")
              "array")
    (array-type (formal-parameter-type (assoc-ref type "items"))))))

(define (run-command command stdin-file stdout-file success-codes)
  "Run @var{command} passing in @var{stdin-file} as the standard input
and writing standard output to @var{stdout-file}. Accept any exit
status in @var{success-codes} as success. Error out otherwise."
  (define (invoke command)
    (unless (member (status:exit-val (apply system* command))
                    success-codes)
      (error "Command invocation failed" command)))
  
  (format (current-error-port)
          "~a$ ~{~a ~}"
          (getcwd)
          command)
  (when stdin-file
    (format (current-error-port) "< ~a " stdin-file))
  (when stdout-file
    (format (current-error-port) "> ~a" stdout-file))
  (newline (current-error-port))
  (let invoke* ((command command)
                (stdin-file stdin-file)
                (stdout-file stdout-file))
    (cond
     (stdin-file
      (with-input-from-file stdin-file
        (cut invoke* command #f stdout-file)))
     (stdout-file
      (with-output-to-file stdout-file
        (cut invoke command)))
     (else
      (with-output-to-port (current-error-port)
        (cut invoke command))))))

(define (sha1-hash file)
  "Return the SHA1 hash of @var{file} as a hexadecimal string."
  (bytevector->base16-string
   (file-hash (lookup-hash-algorithm 'sha1)
              file)))

(define (checksum file)
  "Return the checksum of @var{file} as defined in the CWL specification."
  (string-append "sha1$" (sha1-hash file)))

(define* (evaluate-javascript node expression #:optional (preamble ""))
  "Evaluate javascript @var{expression} using @var{node}. Evaluate
@var{preamble} before evaluating @var{expression}."
  (guard (ex (else (error "Javascript evaluation failed" expression preamble)))
    (call-with-input-pipe (list node
                                (format #f "--eval=~a console.log(\"%j\", ~a)"
                                        preamble expression))
      json->scm)))

(define (evaluate-parameter-reference node expression inputs self runtime expression-lib)
  "Evaluate parameter reference @var{expression} using
@var{node}. @var{inputs}, @var{self} and @var{runtime} provide the
context in which @var{expression} is evaluated. @var{expression-lib}
is a list of expressions evaluated before evaluating @var{expression}."
  (define (set-variable name value)
    (format #f "~a = ~a;"
            name
            (scm->json-string value)))

  (evaluate-javascript node
                       expression
                       (string-join (append expression-lib
                                            (list (set-variable "inputs" inputs)
                                                  (set-variable "self" self)
                                                  (set-variable "runtime" runtime))))))
