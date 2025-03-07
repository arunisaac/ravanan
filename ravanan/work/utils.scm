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

(define-module (ravanan work utils)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (ravanan work vectors)
  #:export (list->dotted-list
            alist=?
            assoc-ref*
            assoc-set
            json-ref
            canonicalize-json
            call-with-temporary-directory
            call-with-input-pipe
            call-with-atomic-output-file))

(define (list->dotted-list lst)
  "Convert @var{lst} of 2-element lists into a true association list of
pairs."
  (map (match-lambda
         ((key value)
          (cons key value)))
       lst))

(define (alist=? alist1 alist2)
  "Return @code{#t} if @var{alist1} and @var{alist2} are equal. Keys and values are
compared using @code{equal?}, and the order of key-value pairs does not matter."
  (lset= equal? alist1 alist2))

(define (assoc-ref* alist key)
  "Return value mapped to @var{key} in @var{alist}. Raise an &assertion
if not found."
  (match (assoc key alist)
    ((_ . value) value)
    (#f (assertion-violation (cons key alist)
                             "Missing key in association list"))))

(define (assoc-set alist . pairs)
  "Functionally set @var{pairs} in @var{alist}. @var{alist} is not
mutated."
  (match pairs
    (((key . value) tail ...)
     (apply assoc-set
            (acons key value
                   (alist-delete key alist))
            tail))
    (() alist)))

(define (json-ref scm . keys)
  "Extract subtree of JSON @var{scm} that is addressed by @var{keys}."
  (match keys
    ((key other-keys ...)
     (apply json-ref
            ((if (list? scm) assoc-ref vector-ref) scm key)
            other-keys))
    (() scm)))

(define (canonicalize-json tree)
  "Canonicalize JSON @var{tree} by recursively sorting objects in lexicographic
order of keys."
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

(define* (call-with-temporary-directory proc
                                        #:optional (parent-directory (getcwd)))
  "Call @var{proc} with a new temporary directory in
@var{parent-directory}, and delete it when @var{proc} returns or exits
non-locally."
  (let ((temporary-directory (mkdtemp (string-append parent-directory "/XXXXXX"))))
    (dynamic-wind (const #t)
                  (cut proc temporary-directory)
                  (lambda ()
                    ;; proc may have moved or even deleted
                    ;; temporary-directory. So, check if it still
                    ;; exists.
                    (when (file-exists? temporary-directory)
                      (delete-file-recursively temporary-directory))))))

(define (call-with-input-pipe command proc)
  "Call @var{proc} with input pipe to @var{command}. @var{command} is a
list of program arguments."
  (match command
    ((prog args ...)
     (let ((port #f))
       (dynamic-wind
         (lambda ()
           (set! port (apply open-pipe* OPEN_READ prog args)))
         (cut proc port)
         (lambda ()
           (unless (zero? (close-pipe port))
             (error "Command invocation failed" command))))))))

(define (fsync fd)
  "Synchronize file descriptor @var{fd} with storage device using the @code{fsync}
system call."
  (let ((return errno ((pointer->procedure int
                                           (foreign-library-pointer (dynamic-link)
                                                                    "fsync")
                                           (list int)
                                           #:return-errno? #t)
                       fd)))
    (unless (zero? return)
      (error "fsync failed" (strerror errno)))))

(define (call-with-atomic-output-file file proc)
  "Call @var{proc} with an output port, data written to which will be atomically
written to @var{file}."
  (let ((temporary-file-port #f)
        (temporary-filename #f))
    (dynamic-wind (lambda ()
                    (set! temporary-file-port
                          (mkstemp (string-append file ".XXXXXX")))
                    (set! temporary-filename
                          (port-filename temporary-file-port)))
                  (cut proc temporary-file-port)
                  (lambda ()
                    (fsync (port->fdes temporary-file-port))
                    (close-port temporary-file-port)
                    (rename-file temporary-filename file)))))
