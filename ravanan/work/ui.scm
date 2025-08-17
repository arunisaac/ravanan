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

(define-module (ravanan work ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%log-level
            log
            log-debug
            log-info
            log-warning
            log-error
            log-critical
            warning
            user-error))

(define %log-level
  (make-parameter #f))

(define (log-level>=? level1 level2)
  "Return @code{#t} if log @var{level1} has greater than or equal severity to log
@var{level2}. Else, return @code{#f}."
  (let ((levels '(debug info warning error critical)))
    (>= (list-index (cut eq? level1 <>) levels)
        (list-index (cut eq? level2 <>) levels))))

(define (log level fmt . args)
  "Log message when current log level is not @code{#f} and @var{level} has greater
than or equal severity to the current log level. @var{fmt} and @var{args} are
arguments to @code{format}."
  (when (and (%log-level)
             (log-level>=? level (%log-level)))
    (apply format (current-error-port) fmt args)
    (newline)))

(define log-debug
  (cut log 'debug <> <...>))

(define log-info
  (cut log 'info <> <...>))

(define log-warning
  (cut log 'warning <> <...>))

(define log-error
  (cut log 'error <> <...>))

(define log-critical
  (cut log 'critical <> <...>))

(define (warning fmt . args)
  "Print warning. @var{fmt} and @var{args} are arguments to format."
  (apply log-warning fmt args))

(define (user-error fmt . args)
  "Print error message and exit with failure. @var{fmt} and @var{args} are
arguments to format."
  (apply log-error fmt args)
  (exit #f))
