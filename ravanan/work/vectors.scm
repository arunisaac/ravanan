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

(define-module (ravanan work vectors)
  #:use-module ((rnrs base) #:select (vector-for-each vector-map))
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-43) #:select (vector-append
                                         (vector-map . vector-map-indexed)
                                         vector-any
                                         vector-every))
  #:use-module ((srfi srfi-43) #:select (vector-fold) #:prefix srfi-43:)
  #:export (vector-map->list
            vector-append-map
            vector-append-map->list
            map->vector
            vector-fold
            vector-filter
            vector-filter-map
            vector-filter-map->list
            vector-remove
            vector-find
            vector-member)
  #:re-export (vector-append
               vector-for-each
               vector-map
               vector-map-indexed
               vector-any
               vector-every))

(define (vector-fold proc init first-vector . other-vectors)
  "Apply @var{proc} to the elements of @var{first-vector} and @var{other-vectors}
to build a result, and return that result. Each @var{proc} call is @code{(proc
result element1 element2 ...)} where @var{result} is the return value from the
previous call to @var{proc}, or @var{init} for the first call."
  (apply srfi-43:vector-fold
         (lambda (_ result . elements)
           (apply proc result elements))
         init
         first-vector
         other-vectors))

(define (vector-map->list proc first-vector . other-vectors)
  "Map @var{proc} over vectors and return a list of the results."
  (reverse
   (apply vector-fold
          (lambda (result . elements)
            (cons (apply proc elements)
                  result))
          (list)
          first-vector
          other-vectors)))

(define (vector-append-map proc first-vector . other-vectors)
  "Map @var{proc} over vectors and return a vector of the results appended
together."
  (apply vector-fold
         (lambda (result . elements)
           (vector-append result
                          (apply proc elements)))
         (vector)
         first-vector
         other-vectors))

(define (vector-append-map->list proc first-vector . other-vectors)
  "Map @var{proc} over vectors and return a list of the results appended
together."
  (apply vector-fold
         (lambda (result . elements)
           (append result
                   (apply proc elements)))
         (list)
         first-vector
         other-vectors))

(define (map->vector proc first-list . other-lists)
  "Map @var{proc} over lists and return a vector of the results."
  (list->vector (apply map proc first-list other-lists)))

(define (vector-filter pred vec)
  "Return a vector with elements from @var{vec} that pass @var{pred}."
  (list->vector (filter pred (vector->list vec))))

(define (vector-filter-map proc vec)
  "Map @var{proc} over @var{vec} and return a vector of the results that are not
@code{#f}."
  (list->vector (filter-map proc (vector->list vec))))

(define (vector-filter-map->list proc vec)
  "Map @var{proc} over @var{vec} and return a list of the results that are not
@code{#f}."
  (filter-map proc (vector->list vec)))

(define (vector-remove proc vec)
  "Return a vector with elements from @var{vec} that fail @var{pred}."
  (vector-filter (negate proc) vec))

(define (vector-find pred vec)
  "Return the first element of @var{vec} that pass @var{pred}. If no such
element exists, return @code{#f}."
  (vector-any (lambda (x)
                (and (pred x)
                     x))
              vec))

(define (vector-member x vec)
  "Return @code{#t} if @var{x} is a member of @var{vec}. Else, return @code{#f}."
  (member x (vector->list vec)))
