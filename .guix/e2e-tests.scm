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
  #:use-module ((gnu packages bioinformatics) #:select (ccwl))
  #:use-module (guix gexp)
  #:use-module (ice-9 match))

(define (e2e-tools-gexp sources-directory)
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
                  (find-files #$sources-directory "\\.scm$")))))

(define e2e-tools
  (computed-file "e2e-tools"
                 (e2e-tools-gexp (local-file "../e2e-tests/tools"
                                             #:recursive? #t))))

(define e2e-test-suite
  (file-union "e2e-test-suite"
              `(("tests.yaml" ,(local-file "../e2e-tests/tests.yaml"))
                ("tools" ,e2e-tools)
                ("jobs" ,(local-file "../e2e-tests/jobs"
                                     #:recursive? #t)))))

(define-public e2e-tests
  (program-file "e2e-tests"
                (cwltest-suite-gexp (file-append e2e-test-suite "/tests.yaml")
                                    (local-file "../e2e-tests/manifest.scm"))))

e2e-tests
