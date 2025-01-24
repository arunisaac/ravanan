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

(define-module (ravanan slurm-api)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (ravanan verbosity)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work utils)
  #:export (submit-job
            job-state))

(define* (slurm-http-request api-endpoint jwt method path
                             #:key (headers '()) body)
  "Make a HTTP request to @var{path} using @var{method} on a slurm
@var{api-endpoint} authenticating using @var{jwt}. Pass body and additional
@var{headers}. Return the JSON response tree as a state-monadic value."
  (state-return
   (let ((response body (http-request (build-uri (uri-scheme api-endpoint)
                                                 #:host (uri-host api-endpoint)
                                                 #:port (uri-port api-endpoint)
                                                 #:path path)
                                      #:method method
                                      #:headers `((X-SLURM-USER-TOKEN . ,jwt)
                                                  ,@headers)
                                      #:body body
                                      #:streaming? #t)))
     (json->scm body))))

(define (check-api-error json)
  "Check @var{json} API response for errors, and raise an exception if any."
  (match (json-ref json "errors")
    (#() json)
    (errors (error "Slurm API error" errors))))

(define (slurm-http-get api-endpoint jwt path)
  "Make a HTTP GET request to @var{path} on a slurm @var{api-endpoint}
authenticating using @var{jwt}. Return the JSON response tree as a state-monadic
value."
  (slurm-http-request api-endpoint jwt 'GET path))

(define (slurm-http-post api-endpoint jwt path body-scm)
  "Make a HTTP POST request to @var{path} on a slurm @var{api-endpoint}
authenticating using @var{jwt}. Convert @var{body-scm} to a JSON document and
pass in as the body of the HTTP request. Return the JSON response tree as a
state-monadic value."
  (slurm-http-request api-endpoint
                      jwt
                      'POST
                      path
                      #:headers '((content-type . (application/json)))
                      #:body (call-with-output-bytevector
                              (cut scm->json body-scm <>))))

(define* (submit-job environment stdout-file stderr-file cpus name script
                     #:key api-endpoint jwt partition nice)
  "Submit job named @var{name} running @var{script} to slurm via @var{api-endpoint}
and authenticating using @var{jwt}. Request slurm @var{partition} and @var{nice}
adjustment if they are not @code{#f}. @var{environment} is an association list
of environment variables to set in the job. @var{stdout-file} and
@var{stderr-file} are files in which to write the stdout and stderr of the job
respectively. @var{cpus} is the number of CPUs (in slurm terminology, a CPU is a
hyperthread; see @url{https://slurm.schedmd.com/faq.html#cpu_count, the Slurm
FAQ}) to request for the job. Return the slurm job ID of the submitted job as a
state-monadic value."
  (define job-spec
    (append `(("name" . ,name)
              ("script" . ,(string-append "#!/bin/bash\n" script))
              ("environment" . ,(list->vector
                                 (map (match-lambda
                                        ((name . value)
                                         (string-append name "=" value)))
                                      environment)))
              ("current_working_directory" . "/")
              ("standard_output" . ,stdout-file)
              ("standard_error" . ,stderr-file)
              ("minimum_cpus" . ,cpus))
            (if partition
                `(("partition" . ,partition))
                '())
            (if nice
                `(("nice" . ,nice))
                '())))

  (state-begin
   (state-return (trace 'slurm-api
                        "submitting script ~a as job ~a" script name))
   (state-let* ((json (slurm-http-post api-endpoint
                                       jwt
                                       "/slurm/v0.0.41/job/submit"
                                       `(("jobs" . #(,job-spec))))))
     (check-api-error json)
     (let ((job-id (json-ref json "job_id")))
       (trace 'slurm-api
              "job ~a assigned id ~a" name job-id)
       (state-return job-id)))))

(define* (job-state job-id #:key api-endpoint jwt)
  "Query the state of slurm @var{job-id} via @var{api-endpoint}
authenticating using @var{jwt}. Return value is one of the symbols
@code{pending}, @code{failed} and @code{completed} encapsulated in the state
monad."
  (state-begin
   (state-return (trace 'slurm-api
                        "polling job id ~a" job-id))
   (state-let* ((response (slurm-http-get api-endpoint
                                          jwt
                                          (string-append "/slurm/v0.0.41/job/"
                                                         (number->string job-id)))))
     (match (json-ref response "errors")
       (#()
        (state-return
         (match (json-ref (find (lambda (job)
                                  (= (json-ref job "job_id")
                                     job-id))
                                (vector->list (json-ref response "jobs")))
                          "job_state")
           (#(job-state)
            (trace 'slurm-api
                   "slurmctld reports ~a state for job ~a" job-state job-id)
            (let ((job-state (string->symbol (string-downcase job-state))))
              (trace 'slurm-api
                     "return ~a state for job ~a" job-state job-id)
              job-state)))))
       (#(errors ...)
        ;; Check in slurmdbd if job has been completed and purged from
        ;; slurmctld's active memory.
        (match (find (lambda (error)
                       (= (json-ref error "error_number")
                          ;; Error number 2017 (Invalid job id specified) may
                          ;; have occurred because the job has completed, has
                          ;; exceeded MinJobAge (as set in slurm.conf) and has
                          ;; therefore been purged from slurmctld's active
                          ;; memory.
                          2017))
                     errors)
          (error-2017
           (trace 'slurm-api
                  (string-append
                   "error number 2017 (invalid job id specified) received"
                   " for job ~a; it is not in slurmctld's active memory;"
                   " checking in slurmdbd")
                  job-id)
           (state-let* ((response
                         (slurm-http-get api-endpoint
                                         jwt
                                         (string-append "/slurmdb/v0.0.41/job/"
                                                        (number->string job-id)))))
             (check-api-error response)
             (state-return
              (match (json-ref (find (lambda (job)
                                       (= (json-ref job "job_id")
                                          job-id))
                                     (vector->list (json-ref response "jobs")))
                               "exit_code" "status")
                (#(job-state)
                 (trace 'slurm-api
                        "slurmdbd reports ~a state for job ~a"
                        job-state job-id)
                 ;; job-state is either "SUCCESS" or "ERROR".
                 (let ((job-state (if (eq? (string->symbol (string-downcase job-state))
                                           'success)
                                      'success
                                      'failed)))
                   (trace 'slurm-api
                          "return ~a state for job ~a" job-state job-id)
                   job-state))))))
          (#f
           (trace 'slurm-api
                  (string-append
                   "slurmdbd does not report error number 2017 for job ~a;"
                   " don't know what to do"))
           (check-api-error response)
           (error "Unknown slurm API error"))))))))
