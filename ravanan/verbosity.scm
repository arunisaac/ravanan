;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2025 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (ravanan verbosity)
  #:use-module (srfi srfi-19)
  #:export (%traces
            trace))

(define %traces
  (make-parameter (list)))

(define (trace subsystem message . args)
  "Print out tracing @var{message} with @var{args} for @var{subsystem}. As a
convention, we expect @var{message} to be in lower case."
  (when (memq subsystem (%traces))
    (display (date->string (current-date) "~5") (current-error-port))
    (display " " (current-error-port))
    (display subsystem (current-error-port))
    (display ": " (current-error-port))
    (apply format (current-error-port) message args)
    (newline (current-error-port))))
