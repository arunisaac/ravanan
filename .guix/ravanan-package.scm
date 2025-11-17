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
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public guile-run64
  (let ((commit "5b2bc2d146e7b58f15e1bc653291b71d8376b763")
        (revision "1"))
    (package
      (name "run64")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.systemreboot.net/run64")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wim4vxqzvwdvmdgx5q7ws03qqj392dh8hcph4yxxkqijnabqgvw"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "prefix=" #$output))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure))))
      (home-page "https://run64.systemreboot.net")
      (synopsis "SRFI-64 test runner for Scheme")
      (description "run64 is a SRFI-64 test runner for Scheme.")
      (license license:gpl3+))))

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
