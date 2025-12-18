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

(define-module (ravanan work command-line-tool)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (json)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work types)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (unsupported-uri-scheme
            unsupported-uri-scheme?
            unsupported-uri-scheme-scheme

            call-with-current-directory
            object-type
            match-type
            glob-match-type
            formal-parameter-type
            run-command
            sha1-hash
            sha1-hash-bytes
            checksum
            location->path
            canonicalize-file-value
            secondary-path
            evaluate-javascript

            command-line-binding
            command-line-binding?
            command-line-binding-position
            command-line-binding-prefix
            command-line-binding-type
            command-line-binding-value
            command-line-binding-item-separator
            command-line-binding->args
            build-command))

(define-condition-type &unsupported-uri-scheme &serious
  unsupported-uri-scheme unsupported-uri-scheme?
  (scheme unsupported-uri-scheme-scheme))

(define (call-with-current-directory curdir thunk)
  "Call THUNK with current directory set to CURDIR. Restore current
directory after THUNK returns."
  (let ((original-current-directory (getcwd)))
    (dynamic-wind (cut chdir curdir)
                  thunk
                  (cut chdir original-current-directory))))

(define (object-type obj)
  "Return the type of @var{obj}."
  (cond
   ((eq? obj 'null) 'null)
   ((boolean? obj) 'boolean)
   ((and (number? obj)
         (inexact? obj))
    'float)
   ((and (number? obj)
         (integer? obj))
    'int)
   ((string? obj) 'string)
   ;; For vector objects, we use the first element to guess at the
   ;; subtype.
   ((vector? obj)
    (match obj
      (#(head _ ...)
       (cwl-array-type (object-type head)))
      (#()
       (cwl-array-type 'Any))))
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
   ;; Treat floats and doubles as the same thing. It is unclear to me why the
   ;; CWL specification needs separate types for these.
   ((eq? type 'double)
    (match-type obj 'float))
   ;; Accept ints as floats too.
   ((eq? type 'float)
    (and (memq (object-type obj)
               (list 'int 'float))
         'float))
   ;; Recursively match type of every element of array.
   ((cwl-array-type? type)
    (and (vector? obj)
         (every (cut match-type <> (cwl-array-type-subtype type))
                (vector->list obj))
         type))
   ;; Match any one of the subtypes of the union type.
   ((cwl-union-type? type)
    (any (cut match-type obj <>)
         (cwl-union-type-subtypes type)))
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
   ((cwl-array-type? type)
    (and (every (lambda (m)
                  (glob-match-type (list m)
                                   (cwl-array-type-subtype type)))
                matches)
         type))
   ((cwl-union-type? type)
    (any (cut glob-match-type matches <>)
         (cwl-union-type-subtypes type)))))

;; TODO: Support all types.
(define (formal-parameter-type type)
  "Return the type described in the CWL @var{type} specification."
  (cond
   ;; Union types
   ((vector? type)
    (apply cwl-union-type
           (map formal-parameter-type (vector->list type))))
   ;; Other types
   ((string? type)
    (string->symbol type))
   ;; Array types
   ((string=? (assoc-ref type "type")
              "array")
    (cwl-array-type (formal-parameter-type (assoc-ref type "items"))))))

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

(define (sha1-hash-bytes file)
  "Return the SHA1 hash of @var{file} as a bytevector."
  (file-hash (lookup-hash-algorithm 'sha1)
             file))

(define (sha1-hash file)
  "Return the SHA1 hash of @var{file} as a hexadecimal string."
  (bytevector->base16-string (sha1-hash-bytes file)))

(define (checksum file)
  "Return the checksum of @var{file} as defined in the CWL specification."
  (string-append "sha1$" (sha1-hash file)))

(define (location->path location)
  "Convert file @var{location} @code{file://} URI to path. Tolerate invalid
locations that are actually paths. Raise an @code{&unsupported-uri-scheme}
condition on unsupported URI schemes."
  (cond
   ;; If location is a file:// URI, parse the URI and return the path part.
   ((string->uri location)
    => (lambda (uri)
         (if (eq? (uri-scheme uri) 'file)
             (uri-path uri)
             (raise-exception (unsupported-uri-scheme (uri-scheme uri))))))
   ;; location is actually a path; return as is.
   (else location)))

(define (canonicalize-file-value value)
  "Canonicalize @code{File} type @var{value} adding missing fields."
  (let* ((path (canonicalize-path
                (or (assoc-ref value "path")
                    (location->path (assoc-ref value "location")))))
         ;; The location field may actually be a path instead of an URI; that's
         ;; invalid. So, unconditionally reconstruct the location URI from path.
         ;; This assumes they are always file:// URIs, but that works for now.
         (location (string-append "file://" path)))
    ;; Populate all fields of the File type value.
    (maybe-assoc-set `(("class" . "File")
                       ("location" . ,location)
                       ("path" . ,path)
                       ("basename" . ,(basename path))
                       ("nameroot" . ,(file-name-stem path))
                       ("nameext" . ,(file-name-extension path))
                       ("size" . ,(stat:size (stat path)))
                       ;; Compute the checksum, but only if it is not provided.
                       ;; If it is provided, trust that it is correct. This
                       ;; avoids costly (think hashing terabytes of data) hash
                       ;; computations causing a long delay before the workflow
                       ;; actually starts running.
                       ("checksum" . ,(or (assoc-ref value "checksum")
                                          (checksum path))))
      (cons "secondaryFiles"
            (maybe-let* ((secondary-files
                          (maybe-assoc-ref (just value) "secondaryFiles")))
              (just (vector-map canonicalize-file-value
                                secondary-files)))))))

(define (secondary-path path secondary-file)
  "Derive path to @var{secondary-file} from primary @var{path}."
  (let ((pattern (assoc-ref* secondary-file "pattern")))
    ;; TODO: Implement ? and ^ characters in SecondaryFileSchema DSL.
    (string-append path pattern)))

(define* (evaluate-javascript node expression #:optional (preamble ""))
  "Evaluate javascript @var{expression} using @var{node}. Evaluate
@var{preamble} before evaluating @var{expression}."
  (guard (ex (else (error "Javascript evaluation failed" expression preamble)))
    (call-with-input-pipe (list node
                                "--use-strict"
                                (format #f "--eval=~a console.log(\"%j\", ~a)"
                                        preamble expression))
      json->scm)))

(define-immutable-record-type <command-line-binding>
  (command-line-binding position prefix type value item-separator)
  command-line-binding?
  (position command-line-binding-position)
  (prefix command-line-binding-prefix)
  (type command-line-binding-type)
  (value command-line-binding-value)
  (item-separator command-line-binding-item-separator))

(define (command-line-binding->args binding)
  "Return a list of arguments for @var{binding}. The returned list may
contain strings or G-expressions. The G-expressions may reference an
@code{inputs-directory} variable that must be defined in the context in which
the G-expressions are inserted."
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
     ((cwl-array-type? type)
      (match (vector->list value)
        ;; Empty arrays should be noops.
        (() (list))
        (elements
         (let ((args (append-map command-line-binding->args
                                 elements)))
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
                      ((int float)
                       (number->string value))
                      ((File)
                       (assoc-ref* value "path"))
                      (else
                       (user-error "Invalid formal input type ~a"
                                   type)))))))))

(define (build-command base-command arguments formal-inputs inputs)
  "Return a list of @code{<command-line-binding>} objects for a
@code{CommandLineTool} class workflow with @var{base-command}, @var{arguments},
@var{formal-inputs} and @var{inputs}. The @code{value} field of the returned
@code{<command-line-binding>} objects may be strings or G-expressions. The
G-expressions may reference @var{inputs} and @var{runtime} variables that must
be defined in the context in which the G-expressions are inserted."
  (define (value->command-line-binding position prefix value)
    (let ((type (object-type value)))
      (cond
       ((cwl-array-type? type)
        (command-line-binding position
                              prefix
                              type
                              (vector-map (cut value->command-line-binding
                                               %nothing
                                               %nothing
                                               <>)
                                          value)
                              %nothing))
       (else
        (command-line-binding position prefix type value %nothing)))))

  (define (argument->command-line-binding i argument)
    (value->command-line-binding (cond
                                  ((assoc-ref argument "position")
                                   => string->number)
                                  (else i))
                                 (maybe-assoc-ref (just argument) "prefix")
                                 (assoc-ref* argument "valueFrom")))

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
            ((cwl-array-type? matched-type)
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
                                         (apply json-ref inputs id)
                                         %nothing)))))))))

  ;; For details of this algorithm, see §4.1 Input binding of the CWL
  ;; 1.2 CommandLineTool specification:
  ;; https://www.commonwl.org/v1.2/CommandLineTool.html#Input_binding
  (append
   ;; Insert elements from baseCommand.
   (vector->list (or base-command
                     (vector)))
   (sort
    (append
     ;; Collect CommandLineBinding objects from arguments; assign a sorting key.
     (vector->list
      (vector-map-indexed argument->command-line-binding
                          (or arguments
                              #())))
     ;; Collect CommandLineBinding objects from the inputs schema; assign a
     ;; sorting key.
     (collect-bindings
      (filter-map (lambda (formal-input)
                    ;; Exclude formal inputs without an inputBinding.
                    (and (assoc "inputBinding" formal-input)
                         (let ((id (assoc-ref formal-input "id")))
                           (list (list id)
                                 ;; The inputs alist has been fully resolved
                                 ;; with default and null values. It is
                                 ;; guaranteed to have a mapping for every input
                                 ;; id.
                                 (assoc-ref inputs id)
                                 (or (assoc-ref formal-input "type")
                                     (user-error "Type of input ~a not specified"
                                                 id))
                                 (maybe-assoc-ref (just formal-input)
                                                  "inputBinding")))))
                  (vector->list formal-inputs))))
    ;; Sort elements using the assigned sorting keys.
    (lambda (binding1 binding2)
      (< (command-line-binding-position binding1)
         (command-line-binding-position binding2))))))
