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

(use-modules (srfi srfi-64)
             (ravanan slurm-api))

(define slurm-state-codes->job-state
  (@@ (ravanan slurm-api)
      slurm-state-codes->job-state))

(test-begin "slurm-api")

(test-equal "Handle PENDING job state"
  'pending
  (slurm-state-codes->job-state #("PENDING")))

(test-equal "Handle RUNNING job state"
  'pending
  (slurm-state-codes->job-state #("RUNNING")))

(test-equal "Handle FAILED job state"
  'failed
  (slurm-state-codes->job-state #("FAILED")))

(test-equal "Handle COMPLETED job state"
  'completed
  (slurm-state-codes->job-state #("COMPLETED")))

(test-equal "Handle COMPLETED job state with additional COMPLETING flag"
  'pending
  (slurm-state-codes->job-state #("COMPLETED" "COMPLETING")))

(test-end "slurm-api")
