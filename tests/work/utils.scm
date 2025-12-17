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

(use-modules (srfi srfi-64)
             (ravanan work utils))

(test-begin "work.utils")

(test-error "json-ref must error out on missing keys" #t
            (json-ref '(("foo" . 1)
                        ("bar" . (("foobar" . 3))))
                      "bar" "fubar"))

(test-end "work.command-line-tool")
