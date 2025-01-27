;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2025 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (ravanan store)
  #:use-module (ice-9 filesystem)
  #:export (%store-files-directory
            %store-data-directory
            %store-logs-directory

            script->store-files-directory
            script->store-data-file
            script->store-stdout-file
            script->store-stderr-file))

(define %store-files-directory
  "files")

(define %store-data-directory
  "data")

(define %store-logs-directory
  "logs")

(define (script->store-files-directory script store)
  "Return the store files directory in @var{store} corresponding to @var{script}
path."
  (expand-file-name (file-name-join* %store-files-directory
                                     (basename script))
                    store))

(define (script->store-data-file script store)
  "Return the store data file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-data-directory
                                     (string-append (basename script) ".json"))
                    store))

(define (script->store-stdout-file script store)
  "Return the store stdout file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append (basename script) ".stdout"))
                    store))

(define (script->store-stderr-file script store)
  "Return the store stderr file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append (basename script) ".stderr"))
                    store))
