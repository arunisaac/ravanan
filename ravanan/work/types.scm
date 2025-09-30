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

(define-module (ravanan work types)
  #:use-module (srfi srfi-9 gnu)
  #:export (cwl-array-type
            cwl-array-type?
            cwl-array-type-subtype
            cwl-union-type
            cwl-union-type?
            cwl-union-type-subtypes))

(define-immutable-record-type <cwl-array-type>
  (cwl-array-type subtype)
  cwl-array-type?
  (subtype cwl-array-type-subtype))

(define-immutable-record-type <union-type>
  (-cwl-union-type subtypes)
  cwl-union-type?
  (subtypes cwl-union-type-subtypes))

(define (cwl-union-type . subtypes)
  (-cwl-union-type subtypes))
