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

(define-module (ravanan propnet)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ravanan monads)
  #:use-module (ravanan work utils)
  #:export (propnet
            propnet-propagators
            propnet-value=?
            propnet-merge-values
            propnet-scheduler
            propagator
            propagator-name
            propagator-proc
            propagator-inputs
            propagator-optional-inputs
            propagator-outputs
            scheduler
            scheduler-schedule
            scheduler-poll
            scheduler-poll-interval
            scheduler-capture-output
            run-propnet))

(define-immutable-record-type <propnet>
  (propnet propagators value=? merge-values scheduler)
  propnet?
  (propagators propnet-propagators)
  (value=? propnet-value=?)
  (merge-values propnet-merge-values)
  (scheduler propnet-scheduler))

(define-immutable-record-type <propagator>
  (propagator name proc inputs optional-inputs outputs)
  propagator?
  (name propagator-name)
  (proc propagator-proc)
  (inputs propagator-inputs)
  (optional-inputs propagator-optional-inputs)
  (outputs propagator-outputs))

(define-immutable-record-type <scheduler>
  (scheduler schedule poll poll-interval capture-output)
  scheduler?
  (schedule scheduler-schedule)
  (poll scheduler-poll)
  (poll-interval scheduler-poll-interval)
  (capture-output scheduler-capture-output))

(define (activate-propagator schedule propagator inputs-alist)
  "Activate @var{propagator} with inputs from @var{inputs-alist}. If some
required inputs are absent, do nothing. Schedule the propagator using
@var{schedule}."
  (match (lset-difference equal?
                          (map (match-lambda
                                 ((input-name . _) input-name))
                               (propagator-inputs propagator))
                          (map (match-lambda
                                 ((input-name . _) input-name))
                               inputs-alist)
                          (propagator-optional-inputs propagator))
    (() (just (schedule (propagator-proc propagator) inputs-alist)))
    (_ %nothing)))

(define (rassoc val alist)
  "Return the pair that maps to @var{val} in @var{alist}. If no such pair
exists, return @code{#f}. @var{val} is compared using @code{equal?}."
  (find (match-lambda
          ((_ . this-value)
           (equal? val this-value)))
        alist))

(define (find-propagator-name propnet name)
  "Return propagator with @var{name} in @var{propnet}."
  (find (lambda (propagator)
          (equal? (propagator-name propagator)
                  name))
        (propnet-propagators propnet)))

(define (run-propnet propnet initial-cell-values)
  "Run @var{propnet} with @var{initial-cell-values} until cell values
stabilize."
  (define scheduler
    (propnet-scheduler propnet))

  (define propagator-state->cell-values
    (match-lambda
      ((propagator-name . state)
       "Convert the output of a completed propagator into new cell values to
add to the inbox."
       (let ((output-mapping
              (propagator-outputs
               (find-propagator-name propnet propagator-name))))
         (map (match-lambda
                ((output-name . value)
                 (cons (or (assoc-ref output-mapping output-name)
                           (assertion-violation output-name "Unknown output"))
                       value)))
              ((scheduler-capture-output scheduler)
               state))))))

  (define (propagator-input-values cells propagator)
    "Return input values for @var{propagator} extracted from @var{cells}."
    (apply maybe-alist
           (map (match-lambda
                  ((propagator-input-name . cell-name)
                   (cons propagator-input-name
                         (maybe-assoc-ref (just cells) cell-name))))
                (propagator-inputs propagator))))
  
  (let loop ((cells (list))
             (cell-values-inbox initial-cell-values)
             ;; Pre-schedule all propagators to ensure we trigger those
             ;; propagators that have no inputs at all.
             (propagators-inbox (propnet-propagators propnet))
             (propagators-in-flight (list)))
    (match cell-values-inbox
      ;; Process one new cell value in inbox.
      (((cell-name . new-cell-value)
        tail-cell-values-inbox ...)
       (if ((propnet-value=? propnet)
            (maybe-assoc-ref (just cells) cell-name)
            (just new-cell-value))
           ;; It's the same value. Nothing to do.
           (loop cells
                 tail-cell-values-inbox
                 propagators-inbox
                 propagators-in-flight)
           ;; Update the cell and activate propagators.
           (let ((cells (maybe-assoc-set cells
                          (cons cell-name
                                ((propnet-merge-values propnet)
                                 (maybe-assoc-ref (just cells) cell-name)
                                 (just new-cell-value))))))
             (loop cells
                   tail-cell-values-inbox
                   ;; Enqueue propagators that depend on cell. Union to avoid
                   ;; scheduling the same propagator more than once.
                   (lset-union eq?
                               propagators-inbox
                               (filter (lambda (propagator)
                                         (rassoc cell-name
                                                 (propagator-inputs propagator)))
                                       (propnet-propagators propnet)))
                   propagators-in-flight))))
      ;; In order to minimize the number of times a propagator is run, it is
      ;; important to start scheduling them only after all cells in
      ;; cell-values-inbox are serviced.
      (()
       (match propagators-inbox
         ;; Schedule one propagator in inbox.
         ((propagator other-propagators ...)
          (loop cells
                cell-values-inbox
                other-propagators
                ;; We don't need to cancel or forget about previous runs of the
                ;; same propagator because cells only "accumulate" information;
                ;; they never remove it.
                (append (maybe-alist
                         (cons (propagator-name propagator)
                               (activate-propagator
                                (scheduler-schedule scheduler)
                                propagator
                                (propagator-input-values cells propagator))))
                        propagators-in-flight)))
         ;; Poll propagators in flight and update cell values if any of them are
         ;; done.
         (()
          (match propagators-in-flight
            ;; All propagators are finished. The propnet has
            ;; stabilized. We are done. Return all cell values.
            (()
             cells)
            (_
             ;; Pause before polling so we don't bother the job server
             ;; too often.
             (sleep (scheduler-poll-interval scheduler))
             (let ((finished-propagators
                    propagators-still-in-flight
                    (partition (match-lambda
                                 ((name . state)
                                  (eq? ((scheduler-poll scheduler)
                                        state)
                                       'completed)))
                               propagators-in-flight)))
               (loop cells
                     (apply assoc-set
                            cell-values-inbox
                            (append-map propagator-state->cell-values
                                        finished-propagators))
                     propagators-inbox
                     propagators-still-in-flight))))))))))
