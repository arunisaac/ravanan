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

(define-module (ravanan ui)
  #:export (warning
            user-error))

(define (warning fmt . args)
  "Print warning. @var{fmt} and @var{args} are arguments to format."
  (apply format (current-error-port) fmt args)
  (newline))

(define (user-error fmt . args)
  "Print error message and exit with failure. @var{fmt} and @var{args} are
arguments to format."
  (apply warning fmt args)
  (exit #f))
