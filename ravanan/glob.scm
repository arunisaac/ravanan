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

(define-module (ravanan glob)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (glob))

(define %glob-nospace 1)
(define %glob-aborted 2)
(define %glob-nomatch 3)

(define (glob pattern)
  "Glob for @var{pattern} and return a list of the filenames found."
  (let* ((glob-type (list size_t '* size_t))
         (result (make-c-struct glob-type
                                (list 0 %null-pointer 0)))
         (return-value ((pointer->procedure
                         int
                         (foreign-library-pointer (dynamic-link)
                                                  "glob")
                         (list '* int '* '*))
                        (string->pointer pattern)
                        0
                        %null-pointer
                        result)))
    (cond
     ((zero? return-value)
      (match (parse-c-struct result glob-type)
        ((count paths _)
         (map pointer->string
              (parse-c-struct paths
                              (make-list count '*))))))
     ((= return-value %glob-nospace)
      (error "glob ran out of memory" pattern))
     ((= return-value %glob-aborted)
      (error "glob read error" pattern))
     ((= return-value %glob-nomatch)
      (list)))))
