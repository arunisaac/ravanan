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

(define-module (e2e-tests)
  #:use-module ((cwl-conformance) #:select (cwltest-suite-gexp))
  #:use-module ((gnu packages bioinformatics) #:select (ccwl) #:prefix guix:)
  #:use-module ((gnu packages guile-xyz) #:select (guile-run64))
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 match))

(define ccwl
  (let ((commit "5f6e0d93bb08446d2c6f99484523697a2bda7b4d")
        (revision "1"))
    (package
      (inherit guix:ccwl)
      (name "ccwl")
      (version (git-version "0.4.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.systemreboot.net/ccwl")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19hwkgj8gsfw24fs682b4wxs5chnwj6hv0rvs23vmq7309mgf242"))))
      (native-inputs
       (modify-inputs (package-native-inputs guix:ccwl)
         (prepend guile-run64))))))

(define (e2e-tools-gexp sources-directory doc-hello-world)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (rnrs io ports)
                     (srfi srfi-26)
                     (ice-9 match)
                     (guix build utils))

        (mkdir #$output)
        ;; Compile ccwl sources.
        (for-each (lambda (source-file)
                    (invoke #$(file-append ccwl "/bin/ccwl")
                            "compile"
                            (string-append "--output="
                                           #$output "/"
                                           (basename source-file ".scm") ".cwl")
                            source-file))
                  (find-files #$sources-directory "\\.scm$"))
        ;; Copy CWL files.
        (for-each (lambda (cwl-file)
                    (copy-file cwl-file
                               (string-append #$output "/" (basename cwl-file))))
                  (find-files #$sources-directory "\\.cwl$"))
        ;; Copy Hello World workflow from documentation.
        (copy-file #$doc-hello-world
                   (string-append #$output "/doc-hello-world.cwl")))))

(define e2e-tools
  (computed-file "e2e-tools"
                 (e2e-tools-gexp (local-file "../e2e-tests/tools"
                                             #:recursive? #t)
                                 (local-file "../doc/hello-world.cwl"))))

(define e2e-jobs
  (directory-union "e2e-jobs"
                   (list (local-file "../e2e-tests/jobs"
                                     #:recursive? #t)
                         (file-union "doc-hello-world-inputs"
                                     `(("doc-hello-world.yaml"
                                        ,(local-file "../doc/hello-world-inputs.yaml")))))))

(define e2e-test-suite
  (file-union "e2e-test-suite"
              `(("tests.yaml" ,(local-file "../e2e-tests/tests.yaml"))
                ("tools" ,e2e-tools)
                ("jobs" ,e2e-jobs))))

(define-public e2e-tests
  (program-file "e2e-tests"
                (cwltest-suite-gexp (file-append e2e-test-suite "/tests.yaml")
                                    (local-file "../e2e-tests/manifest.scm"))))

e2e-tests
