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

(define-module (ravanan work utils)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (list->dotted-list
            alist=?
            assoc-ref*
            assoc-set
            call-with-temporary-directory
            call-with-input-pipe))

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
