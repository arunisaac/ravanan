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

(define-module (ravanan single-machine)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work ui)
  #:export (submit-job))

(define (submit-job environment stdout-file stderr-file script)
  "Submit job running @var{script} to the single machine batch system.
@var{environment} is an association list of environment variables to set in the
job. @var{stdout-file} and @var{stderr-file} are files in which to write the
stdout and stderr of the job respectively. Return @code{#t} if job succeeded,
else @code{#f}. The return value is state-monadic."
  (state-return
   (begin
     (for-each (match-lambda
                 ((name . value)
                  (setenv name value)))
               environment)
     (log-info "Running ~a~%"
               script)
     (zero? (with-output-to-file stdout-file
              (lambda ()
                (with-error-to-file stderr-file
                  (cut system* script))))))))
