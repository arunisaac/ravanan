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

(define-module (ravanan utils)
  #:use-module (rnrs base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (assoc-ref*
            json-ref
            call-with-temporary-file))

(define (assoc-ref* alist key)
  "Return value mapped to @var{key} in @var{alist}. Raise an &assertion
if not found."
  (match (assoc key alist)
    ((_ . value) value)
    (#f (assertion-violation (cons key alist)
                             "Missing key in association list"))))

(define (json-ref scm . keys)
  "Extract subtree of JSON @var{scm} that is addressed by @var{keys}."
  (match keys
    ((key other-keys ...)
     (apply json-ref
            ((if (list? scm) assoc-ref vector-ref) scm key)
            other-keys))
    (() scm)))

(define* (call-with-temporary-file proc #:optional (parent-directory (getcwd)))
  "Call @var{proc} with an output port to a new temporary file in
@var{parent-directory}, and delete it when @var{proc} returns or exits
non-locally."
  (let ((temporary-file-port (mkstemp (string-append parent-directory "/XXXXXX"))))
    (dynamic-wind (const #t)
                  (cut proc temporary-file-port)
                  (lambda ()
                    (delete-file (port-filename temporary-file-port))))))
