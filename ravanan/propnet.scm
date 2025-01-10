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

(define-module (ravanan propnet)
  #:use-module ((rnrs base) #:select (assertion-violation))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ravanan work monads)
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
            scheduler-capture-output
            schedule-propnet
            poll-propnet
            capture-propnet-output))

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
  (scheduler schedule poll capture-output)
  scheduler?
  (schedule scheduler-schedule)
  (poll scheduler-poll)
  (capture-output scheduler-capture-output))

(define-immutable-record-type <propnet-state>
  (propnet-state propnet cells cells-inbox propagators-in-flight propagators-inbox)
  propnet-state?
  (propnet propnet-state-propnet)
  (cells propnet-state-cells)
  (cells-inbox propnet-state-cells-inbox)
  (propagators-in-flight propnet-state-propagators-in-flight)
  (propagators-inbox propnet-state-propagators-inbox))

(define (partition-map pred proc lst)
  "Partition @var{lst} into two lists using @var{pred} like @code{partition}. Then,
map @var{proc} over both the lists and return the resulting lists."
  (let ((true-list false-list (partition pred lst)))
    (values (map proc true-list)
            (map proc false-list))))

(define (activate-propagator scheduler propagator inputs-alist)
  "Activate @var{propagator} with inputs from @var{inputs-alist}. If some
required inputs are absent, do nothing. Schedule the propagator using
@var{scheduler}."
  (match (lset-difference equal?
                          (map (match-lambda
                                 ((input-name . _) input-name))
                               (propagator-inputs propagator))
                          (map (match-lambda
                                 ((input-name . _) input-name))
                               inputs-alist)
                          (propagator-optional-inputs propagator))
    (() (just ((scheduler-schedule scheduler)
               (propagator-proc propagator) inputs-alist scheduler)))
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

(define (schedule-propnet propnet initial-cell-values)
  "Start @var{propnet} with @var{initial-cell-values}, and return
@code{<propnet-state>} object."
  (propnet-state propnet
                 (list)
                 initial-cell-values
                 (list)
                 ;; Pre-schedule all propagators to ensure we
                 ;; trigger those propagators that have no inputs
                 ;; at all.
                 (propnet-propagators propnet)))

(define (poll-propnet state)
  "Poll propagator network @var{state}. Return two values---a status symbol (either
@code{completed} or @code{pending}) and the current state of the propagator
network."
  (define propnet
    (propnet-state-propnet state))

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

  ;; We implement propagator networks as a state machine. The state consists of
  ;; the current values of all the cells and the list of all propagators
  ;; currently in flight. Each iteration of loop represents one state
  ;; transition. This is a very functional approach. Propagator network
  ;; implementations don't necessarily have to be mutational.
  (let loop ((cells (propnet-state-cells state))
             (cell-values-inbox (propnet-state-cells-inbox state))
             (propagators-inbox (propnet-state-propagators-inbox state))
             (propagators-in-flight (propnet-state-propagators-in-flight state)))
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
         ;; Poll propagators in flight and update cell values if any of them are
         ;; done.
         (()
          (match propagators-in-flight
            ;; All propagators are finished. The propnet has stabilized. We are
            ;; done. Return all cell values.
            (()
             (values 'completed
                     (propnet-state propnet
                                    cells
                                    cell-values-inbox
                                    propagators-in-flight
                                    propagators-inbox)))
            ;; Propagators are still in flight. Check if any of them have
            ;; completed.
            (_
             (let ((finished-propagators
                    propagators-still-in-flight
                    (partition-map (match-lambda
                                     ((_ _ 'completed) #t)
                                     (_ #f))
                                   (match-lambda
                                     ((name state _)
                                      (cons name state)))
                                   (map (match-lambda
                                          ((name . state)
                                           (let ((status state ((scheduler-poll scheduler)
                                                                state)))
                                             (list name state status))))
                                        propagators-in-flight))))
               (match finished-propagators
                 ;; None of the propagators we checked have completed. Return a
                 ;; pending state.
                 (()
                  (values 'pending
                          (propnet-state propnet
                                         cells
                                         cell-values-inbox
                                         propagators-still-in-flight
                                         propagators-inbox)))
                 ;; Some of the propagators we checked have completed. Enqueue
                 ;; their outputs in the cells inbox and loop.
                 (_
                  (loop cells
                        (apply assoc-set
                               cell-values-inbox
                               (append-map propagator-state->cell-values
                                           finished-propagators))
                        propagators-inbox
                        propagators-still-in-flight)))))))
         ;; Schedule propagators in inbox.
         (_
          (loop cells
                cell-values-inbox
                (list)
                ;; We don't need to cancel or forget about previous runs of the
                ;; same propagator because cells only *accumulate* information;
                ;; they never remove it. Any previous runs of the same
                ;; propagator will only *add to* the information in the output
                ;; cells. Previous runs may be closer to completion and taking
                ;; advantage of their output may allow later stages to start
                ;; running sooner, thus improving throughput. In our CWL
                ;; application of propnets, this will never result in the same
                ;; step being recomputed; so this approach does not come at a
                ;; higher computational cost.
                (append (append-map (lambda (propagator)
                                      (maybe-alist
                                       (cons (propagator-name propagator)
                                             (activate-propagator
                                              scheduler
                                              propagator
                                              (propagator-input-values cells propagator)))))
                                    propagators-inbox)
                        propagators-in-flight))))))))

(define (capture-propnet-output state)
  "Return output of propagator network @var{state}."
  (propnet-state-cells state))
