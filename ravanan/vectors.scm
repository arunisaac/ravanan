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

(define-module (ravanan vectors)
  #:use-module ((rnrs base) #:select (vector-for-each vector-map))
  #:use-module ((srfi srfi-43) #:select (vector-append
                                         (vector-map . vector-map-indexed)
                                         vector-any
                                         vector-fold))
  #:export (vector-map->list
            vector-append-map->list
            map->vector
            vector-filter
            vector-remove
            vector-find)
  #:re-export (vector-append
               vector-for-each
               vector-map
               vector-map-indexed
               vector-any))

(define (vector-map->list proc first-vector . other-vectors)
  "Map @var{proc} over vectors and return a list of the results."
  (reverse
   (apply vector-fold
          (lambda (_ result . elements)
            (cons (apply proc elements)
                  result))
          (list)
          first-vector
          other-vectors)))

(define (vector-append-map->list proc first-vector . other-vectors)
  "Map @var{proc} over vectors and return a list of the results appended
together."
  (apply vector-fold
         (lambda (_ result . elements)
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
