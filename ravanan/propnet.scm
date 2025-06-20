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
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work utils)
  #:export (propnet
            propnet?
            propnet-propagators
            propnet-value=?
            propnet-merge-values
            propnet-scheduler
            propagator
            propagator?
            propagator-name
            propagator-proc
            propagator-inputs
            propagator-optional-inputs
            propagator-outputs
            scheduler
            scheduler?
            scheduler-schedule
            scheduler-poll
            scheduler-capture-output
            schedule-propnet
            state+status
            state+status?
            state+status-state
            state+status-status
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

(set-record-type-printer! <propagator>
                          (lambda (record port)
                            (display "#<<propagator> " port)
                            (write (propagator-name record) port)
                            (display ">" port)))

(define-immutable-record-type <scheduler>
  (scheduler schedule poll capture-output)
  scheduler?
  (schedule scheduler-schedule)
  (poll scheduler-poll)
  (capture-output scheduler-capture-output))

(define-immutable-record-type <state+status>
  (state+status state status)
  state+status?
  (state state+status-state)
  (status state+status-status))

(define-immutable-record-type <propnet-state>
  (propnet-state propnet cells cells-inbox propagators-in-flight propagators-inbox)
  propnet-state?
  (propnet propnet-state-propnet)
  (cells propnet-state-cells)
  (cells-inbox propnet-state-cells-inbox)
  (propagators-in-flight propnet-state-propagators-in-flight)
  (propagators-inbox propnet-state-propagators-inbox))

(set-record-type-printer! <propnet-state>
                          (lambda (record port)
                            (display "#<<propnet-state> cells: " port)
                            (write (run-with-state (propnet-state-cells record)) port)
                            (display " cells-inbox: " port)
                            (write (run-with-state (propnet-state-cells-inbox record)) port)
                            (display " propagators-in-flight: " port)
                            (write (run-with-state (propnet-state-propagators-in-flight record)) port)
                            (display " propagators-inbox: " port)
                            (write (run-with-state (propnet-state-propagators-inbox record)) port)
                            (display ">" port)))

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
  "Start @var{propnet} with @var{initial-cell-values}, and return a state-monadic
@code{<propnet-state>} object."
  (state-return
   (propnet-state propnet
                  (state-return (list))
                  (state-return initial-cell-values)
                  (state-return (list))
                  ;; Pre-schedule all propagators to ensure we trigger those
                  ;; propagators that have no inputs at all.
                  (state-return (propnet-propagators propnet)))))

(define (poll-propnet state)
  "Poll propagator network @var{state}. Return a state-monadic
@code{<state+status>} object containing two values---the current state of the
propagator network and a status symbol (either @code{completed} or
@code{pending})."
  (define propnet
    (propnet-state-propnet state))

  (define scheduler
    (propnet-scheduler propnet))

  (define propagator-state->cell-values
    (match-lambda
      ((propagator-name . state)
       "Convert the output of a completed propagator into new cell values to
add to the inbox. The return value is a state-monadic list."
       (let ((output-mapping
              (propagator-outputs
               (find-propagator-name propnet propagator-name))))
         (state-let* ((captured-outputs ((scheduler-capture-output scheduler)
                                         state)))
           (state-return
            (map (match-lambda
                   ((output-name . value)
                    (cons (or (assoc-ref output-mapping output-name)
                              (assertion-violation output-name "Unknown output"))
                          value)))
                 captured-outputs)))))))

  (define (propagator-input-values cells propagator)
    "Return input values for @var{propagator} extracted from @var{cells}."
    (apply maybe-alist
           (map (match-lambda
                  ((propagator-input-name . cell-name)
                   (cons propagator-input-name
                         (maybe-assoc-ref (just cells) cell-name))))
                (propagator-inputs propagator))))

  (define (schedule-propagators propagators cells)
    "Schedule all propagators among @var{propagators} whose inputs are present in
@var{cells}. Return an association list mapping scheduled propagator names to
their monadic states."
    (state-map (match-lambda
                 ((name . mpropagator-state)
                  (state-let* ((propagator-state mpropagator-state))
                    (state-return (cons name propagator-state)))))
               (append-map (lambda (propagator)
                             (maybe-alist
                              (cons (propagator-name propagator)
                                    (activate-propagator
                                     scheduler
                                     propagator
                                     (propagator-input-values cells propagator)))))
                           propagators)))

  ;; We implement propagator networks as a state machine. The state consists of
  ;; the current values of all the cells and the list of all propagators
  ;; currently in flight. Each iteration of loop represents one state
  ;; transition. This is a very functional approach. Propagator network
  ;; implementations don't necessarily have to be mutational.
  (let loop ((mcells (propnet-state-cells state))
             (mcell-values-inbox (propnet-state-cells-inbox state))
             (mpropagators-inbox (propnet-state-propagators-inbox state))
             (mpropagators-in-flight (propnet-state-propagators-in-flight state)))
    (state-let* ((cells mcells)
                 (cell-values-inbox mcell-values-inbox)
                 (propagators-inbox mpropagators-inbox)
                 (propagators-in-flight mpropagators-in-flight))
      (match cell-values-inbox
        ;; Process one new cell value in inbox.
        (((cell-name . new-cell-value)
          tail-cell-values-inbox ...)
         (if ((propnet-value=? propnet)
              (maybe-assoc-ref (just cells) cell-name)
              (just new-cell-value))
             ;; It's the same value. Nothing to do.
             (loop mcells
                   (state-return tail-cell-values-inbox)
                   mpropagators-inbox
                   mpropagators-in-flight)
             ;; Update the cell and activate propagators.
             (loop (state-return (maybe-assoc-set cells
                                   (cons cell-name
                                         ((propnet-merge-values propnet)
                                          (maybe-assoc-ref (just cells) cell-name)
                                          (just new-cell-value)))))
                   (state-return tail-cell-values-inbox)
                   ;; Enqueue propagators that depend on cell. Union to avoid
                   ;; scheduling the same propagator more than once.
                   (state-return
                    (lset-union eq?
                                propagators-inbox
                                (filter (lambda (propagator)
                                          (rassoc cell-name
                                                  (propagator-inputs propagator)))
                                        (propnet-propagators propnet))))
                   mpropagators-in-flight)))
        ;; In order to minimize the number of times a propagator is run, it is
        ;; important to start scheduling them only after all cells in
        ;; cell-values-inbox are serviced.
        (()
         (match propagators-inbox
           ;; Poll propagators in flight and update cell values if any of them
           ;; are done.
           (()
            (match propagators-in-flight
              ;; All propagators are finished. The propnet has stabilized. We
              ;; are done. Return state and completed status.
              (()
               (state-return
                (state+status (propnet-state propnet
                                             mcells
                                             mcell-values-inbox
                                             mpropagators-in-flight
                                             mpropagators-inbox)
                              'completed)))
              ;; Propagators are still in flight. Check if any of them have
              ;; completed.
              (_
               (state-let* ((propagator-states
                             (state-map (match-lambda
                                          ((name . state)
                                           (state-let* ((state+status
                                                         ((scheduler-poll scheduler) state)))
                                             (state-return
                                              (list name
                                                    (state+status-state state+status)
                                                    (state+status-status state+status))))))
                                        propagators-in-flight)))
                 (let ((mfinished-propagators
                        mpropagators-still-in-flight
                        (call-with-values (cut partition
                                               (match-lambda
                                                 ((_ _ 'completed) #t)
                                                 (_ #f))
                                               propagator-states)
                          (lambda lists
                            (apply values
                                   (map (cut state-map
                                             (match-lambda
                                               ((name state _)
                                                (state-return (cons name state))))
                                             <>)
                                        lists))))))
                   (state-let* ((finished-propagators mfinished-propagators))
                     (match finished-propagators
                       ;; None of the propagators we checked have completed.
                       ;; Return a pending state.
                       (()
                        (state-return
                         (state+status (propnet-state propnet
                                                      mcells
                                                      mcell-values-inbox
                                                      mpropagators-still-in-flight
                                                      mpropagators-inbox)
                                       'pending)))
                       ;; Some of the propagators we checked have completed.
                       ;; Enqueue their outputs in the cells inbox and loop.
                       (_
                        (loop mcells
                              (state-let* ((new-cell-values
                                            (state-append-map propagator-state->cell-values
                                                              finished-propagators)))
                                (state-return (apply assoc-set
                                                     cell-values-inbox
                                                     new-cell-values)))
                              mpropagators-inbox
                              mpropagators-still-in-flight)))))))))
           ;; Schedule propagators in inbox.
           (_
            (loop mcells
                  mcell-values-inbox
                  (state-return (list))
                  ;; We don't need to cancel or forget about previous runs of
                  ;; the same propagator because cells only *accumulate*
                  ;; information; they never remove it. Any previous runs of the
                  ;; same propagator will only *add to* the information in the
                  ;; output cells. Previous runs may be closer to completion and
                  ;; taking advantage of their output may allow later stages to
                  ;; start running sooner, thus improving throughput. In our CWL
                  ;; application of propnets, this will never result in the same
                  ;; step being recomputed; so this approach does not come at a
                  ;; higher computational cost.
                  (state-let* ((new-propagators-in-flight
                                (schedule-propagators propagators-inbox
                                                      cells)))
                    (state-return
                     (append new-propagators-in-flight
                             propagators-in-flight)))))))))))

(define (capture-propnet-output state)
  "Return output of propagator network @var{state} as a state-monadic value."
  (propnet-state-cells state))
