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
  (let ((commit "badcc3df8488c95359d30f907c8da043fcc0c455")
        (revision "0"))
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
                  "1zwvjrvdvph7kgpd8scyn2masgy4dci3q7ndh2907nl8rp9skbq7"))))
      (native-inputs
       (modify-inputs (package-native-inputs guix:ccwl)
         (prepend guile-run64))))))

(define (e2e-tools-gexp sources-directory doc-hello-world)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (rnrs io ports)
                     (srfi srfi-26)
                     (ice-9 match)
                     (ice-9 popen)
                     (guix build utils))

        (define (call-with-input-pipe command proc)
          (match command
            ((prog args ...)
             (let ((port #f))
               (dynamic-wind
                 (lambda ()
                   (set! port (apply open-pipe* OPEN_READ prog args)))
                 (cut proc port)
                 (lambda ()
                   (unless (zero? (close-pipe port))
                     (error "Command invocation failed" command))))))))

        (mkdir #$output)
        ;; Compile ccwl sources.
        (for-each (lambda (source-file)
                    (call-with-output-file (string-append #$output
                                                          "/"
                                                          (basename source-file ".scm")
                                                          ".cwl")
                      (cut display
                           (call-with-input-pipe (list #$(file-append ccwl "/bin/ccwl")
                                                       "compile"
                                                       source-file)
                             get-string-all)
                           <>)))
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
