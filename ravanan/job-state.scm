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

;;; Commentary:

;; This module isolates the job-state record types for the various batch systems
;; and exports only a generic interface to them. In addition, it also exports
;; constructors for the various job-state record types.

;;; Code:

(define-module (ravanan job-state)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ravanan batch-system)
  #:use-module (ravanan slurm-api)
  #:use-module (ravanan work monads)
  #:export (single-machine-job-state
            slurm-job-state

            job-state-script
            job-state-status))

(define-immutable-record-type <single-machine-job-state>
  (single-machine-job-state script success?)
  single-machine-job-state?
  (script single-machine-job-state-script)
  (success? single-machine-job-state-success?))

(define-immutable-record-type <slurm-job-state>
  (slurm-job-state script job-id)
  slurm-job-state?
  (script slurm-job-state-script)
  (job-id slurm-job-state-job-id))

(define (job-state-script state)
  ((cond
    ((single-machine-job-state? state)
     single-machine-job-state-script)
    ((slurm-job-state? state)
     slurm-job-state-script))
   state))

(define* (job-state-status state batch-system)
  "Return current status and updated state of job with @var{state} on
@var{batch-system}. The status is one of the symbols @code{completed},
@code{failed} or @code{pending}."
  (cond
   ;; Single machine jobs are run synchronously. So, they return success or
   ;; failure immediately.
   ((single-machine-job-state? state)
    (if (single-machine-job-state-success? state)
        'completed
        'failed))
   ;; Poll slurm for job state.
   ((slurm-job-state? state)
    (run-with-state
     (job-state (slurm-job-state-job-id state)
                #:api-endpoint (slurm-api-batch-system-endpoint batch-system)
                #:jwt (slurm-api-batch-system-jwt batch-system))))
   ;; For list states, poll each state element and return 'completed only if all
   ;; state elements have completed.
   ((list? state)
    (or (every (lambda (state-element)
                 (case (job-state-status state-element batch-system)
                   ((completed) => identity)
                   (else #f)))
               state)
        'pending))))
