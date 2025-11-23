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

(define-module (ravanan-package)
  #:use-module ((gnu packages bioinformatics) #:select (ravanan) #:prefix guix:)
  #:use-module ((gnu packages guile-xyz) #:select (guile-run64))
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ravanan
  (package
    (inherit guix:ravanan)
    (source (local-file ".."
                        "ravanan-checkout"
                        #:recursive? #t
                        #:select? (lambda (file stat)
                                    ;; If .guix is included, changes to other
                                    ;; files under .guix—such as the CWL
                                    ;; conformance tests—unnecessarily trigger a
                                    ;; rebuild of ravanan. This could be a
                                    ;; nuisance when hacking on the CWL
                                    ;; conformance test scripts.
                                    (and (not (string-contains file "/.guix/"))
                                         (not (string-contains file "/e2e-tests/"))
                                         ((or (git-predicate (dirname (current-source-directory)))
                                              (const #t))
                                          file stat)))))
    (native-inputs
     (modify-inputs (package-native-inputs guix:ravanan)
       (prepend guile-run64)))))

ravanan
