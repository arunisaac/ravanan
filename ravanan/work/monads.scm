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

(define-module (ravanan work monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:export (maybe-bind
            just
            %nothing
            nothing?
            maybe-let*
            from-maybe
            maybe->list
            maybe-find
            maybe-vector-find
            maybe-assoc-ref
            maybe-assoc-set
            maybe-alist

            state-bind
            state-return
            state-let*
            state-begin
            state-sequence
            current-state
            set-current-state
            run-with-state))

(define-immutable-record-type <monad>
  (monad bind return)
  monad?
  (bind monad-bind)
  (return monad-return))

(define-syntax mlet*
  (syntax-rules ()
    ((_ monad-type () body ...)
     (begin
       body ...))
    ((_ monad-type ((var mvalue) bindings ...) body ...)
     ((monad-bind monad-type)
      mvalue
      (lambda (var)
        (mlet* monad-type (bindings ...)
            body ...))))))

(define-syntax mbegin
  (syntax-rules ()
    ((_ monad-type expression)
     expression)
    ((_ monad-type first-expression body ...)
     ((monad-bind monad-type)
      first-expression
      (lambda _
        body ...)))))

(define (sequence monad-type mvalues)
  "Convert a list of monadic @var{mvalues} in @var{monad-type} into a monadic list
of values."
  (let ((return (monad-return monad-type)))
    (mlet* monad-type ((reverse-list
                        (fold (lambda (mvalue mresult)
                                (mlet* monad-type ((value mvalue)
                                                   (result mresult))
                                  (return (cons value result))))
                              (return (list))
                              mvalues)))
      (return (reverse reverse-list)))))

(define-immutable-record-type <maybe>
  (maybe value valid?)
  maybe?
  (value maybe-value)
  (valid? maybe-valid?))

(set-record-type-printer! <maybe>
                          (lambda (record port)
                            (if (maybe-valid? record)
                                (begin
                                  (display "[Just " port)
                                  (write (maybe-value record) port)
                                  (display "]" port))
                                (display "Nothing" port))))

(define %maybe-monad
  (monad (lambda (m proc)
           (if (maybe-valid? m)
               (proc (maybe-value m))
               %nothing))
         (cut maybe <> #t)))

(define maybe-bind
  (monad-bind %maybe-monad))

(define just
  (monad-return %maybe-monad))

(define %nothing
  (maybe #f #f))

(define nothing?
  (negate maybe-valid?))

(define-syntax-rule (maybe-let* bindings body ...)
  (mlet* %maybe-monad bindings
    body ...))

;; We define from-maybe as a macro since default should not be eagerly
;; evaluated.
(define-syntax-rule (from-maybe mvalue default)
  "If maybe-monadic @var{mvalue} is @code{%nothing}, return
@var{default}. Else, return the value contained within."
  (if (maybe-valid? mvalue)
      (maybe-value mvalue)
      default))

(define (maybe->list mvalue)
  "If maybe-monadic @var{mvalue} is @code{%nothing}, return an empty
list. Else, return a singleton list of the value contained within."
  (if (maybe-valid? mvalue)
      (list (maybe-value mvalue))
      (list)))

(define (maybe-find pred lst)
  "Find the first element of @var{lst} that satisfies @var{pred}, and
return it as a maybe value. Return @code{%nothing} if no such element
is found."
  (match (find-tail pred lst)
    ((element . _) (just element))
    (#f %nothing)))

(define (maybe-vector-find pred vec)
  "Find the first element of @var{vec} that satisfies @var{pred}, and
return it as a maybe value. Return @code{%nothing} if no such element
is found."
  (or (vector-any (lambda (x)
                    (and (pred x)
                         (just x)))
                  vec)
      %nothing))

(define (maybe-assoc-ref maybe-alist . keys)
  "Return the value addressed by @var{keys} in maybe-monadic
@var{maybe-alist}. The return value is also maybe-monadic."
  (fold (lambda (key result)
          (maybe-bind result
                      (lambda (alist)
                        (match (assoc key alist)
                          ((_ . value) (just value))
                          (_ %nothing)))))
        maybe-alist
        keys))

(define (assoc-set alist . pairs)
  "Return a new association alist with keys in @var{alist} set to values
as specified in @var{pairs}. Each element of @var{pairs} maps an
atomic key to a value. Keys may also be a list of atomic values
specifying a path through the association list tree."
  (fold (lambda (pair result)
          (match pair
            (((key) . value)
             (assoc-set alist (cons key value)))
            (((head-key tail-key-path ...) . value)
             (acons head-key
                    (assoc-set (assoc-ref result head-key)
                               (cons tail-key-path value))
                    (alist-delete head-key result)))
            ((key . value)
             (acons key value
                    (alist-delete key result)))))
        alist
        pairs))

(define (maybe-assoc-set alist . key-maybe-value-pairs)
  "Like @code{assoc-set}, but values in @var{key-maybe-value-pairs} are
maybe-monadic."
  (apply assoc-set
         alist
         (filter-map (match-lambda
                       ((key . value)
                        (and (maybe-valid? value)
                             (cons key (maybe-value value)))))
                     key-maybe-value-pairs)))

(define maybe-alist
  (cut maybe-assoc-set (list) <...>))

(define-immutable-record-type <mstate>
  (mstate state value)
  mstate?
  (state mstate-state)
  (value mstate-value))

(define %state-monad
  (monad (lambda (mvalue mproc)
           (lambda (state)
             (match (mvalue state)
               (($ <mstate> next-state value)
                ((mproc value) next-state)))))
         (lambda (value)
           (cut mstate <> value))))

(define state-bind
  (monad-bind %state-monad))

;; We expose only a macro interface to the return function of the state monad
;; since we want its argument to be evaluated lazily. If we exposed a functional
;; interface, then a state-return used in isolation (that is, without a
;; state-bind) would have its argument evaluated eagerly and side-effects would
;; occur before the monad is actually run. FIXME: Unfortunately, this means that
;; we duplicate the definition of return in %state-monad.
(define-syntax-rule (state-return value)
  ((lambda (delayed-value)
     (cut mstate <> (force delayed-value)))
   (delay value)))

(define-syntax-rule (state-let* bindings body ...)
  (mlet* %state-monad bindings
    body ...))

(define-syntax-rule (state-begin body ...)
  (mbegin %state-monad
    body ...))

(define state-sequence
  (cut sequence %state-monad <>))

(define (current-state)
  "Return the current state as a state-monadic value."
  (lambda (state)
    (mstate state state)))

(define (set-current-state new-state)
  "Set @var{new-state} as the state."
  (cut mstate new-state <>))

(define* (run-with-state mvalue #:optional initial-state)
  "Run state-monadic value @var{mvalue} starting with @var{initial-state}. Return
two values---the value encapsulated in @var{mvalue} and the final state."
  (match (mvalue initial-state)
    (($ <mstate> state value)
     (values value state))))
