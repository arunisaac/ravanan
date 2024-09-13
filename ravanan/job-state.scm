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

;;; Commentary:

;; This module isolates the various job-state record types and exports only a
;; generic interface to them. In addition, it also exports constructors for the
;; various job-state record types.

;;; Code:

(define-module (ravanan job-state)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ravanan slurm-api)
  #:use-module (ravanan work vectors)
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

(define* (job-state-status state #:key slurm-api-endpoint slurm-jwt)
  "Return current status of job with @var{state}---one of the symbols
@code{completed}, @code{failed} or @code{pending}.

@var{slurm-api-endpoint} and @var{slurm-jwt} are the same as in
@code{run-workflow} from @code{(ravanan workflow)}."
  (cond
   ;; Single machine jobs are run synchronously. So, they return success or
   ;; failure immediately.
   ((single-machine-job-state? state)
    (if (single-machine-job-state-success? state)
        'completed
        'failed))
   ;; Poll slurm for job state.
   ((slurm-job-state? state)
    (job-state (slurm-job-state-job-id state)
               #:api-endpoint slurm-api-endpoint
               #:jwt slurm-jwt))
   ;; For vector states, poll each state element and return 'completed only if
   ;; all state elements have completed.
   ((vector? state)
    (or (vector-every (lambda (state-element)
                        (case (job-state-status state-element)
                          ((completed) => identity)
                          (else #f)))
                      state)
        'pending))))
