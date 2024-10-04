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

(define-module (ravanan work monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:export (maybe-monad
            maybe-bind
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
            maybe-alist))

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
