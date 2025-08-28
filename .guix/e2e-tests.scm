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

(define (ccwl-compile source-file)
  #~(begin
      (use-modules (rnrs io ports)
                   (srfi srfi-26)
                   (ice-9 match)
                   (ice-9 popen))

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

      (call-with-output-file #$output
        (cut display
             (call-with-input-pipe '(#$(file-append ccwl "/bin/ccwl")
                                     "compile"
                                     #$source-file)
               get-string-all)
             <>))))

(define e2e-tools-ccwl-sources
  `(("capture-output-file.scm"
     . ,(local-file "../e2e-tests/tools/capture-output-file.scm"))
    ("capture-output-file-with-parameter-reference.scm"
     . ,(local-file "../e2e-tests/tools/capture-output-file-with-parameter-reference.scm"))
    ("capture-stdout"
     . ,(local-file "../e2e-tests/tools/capture-stdout.scm"))
    ("checksum"
     . ,(local-file "../e2e-tests/tools/checksum.scm"))
    ("decompress-compile-run"
     . ,(local-file "../e2e-tests/tools/decompress-compile-run.scm"))
    ("inline-javascript-requirement"
     . ,(local-file "../e2e-tests/tools/inline-javascript-requirement.scm"))
    ("hello-world.scm"
     . ,(local-file "../e2e-tests/tools/hello-world.scm"))
    ("pass-stdin"
     . ,(local-file "../e2e-tests/tools/pass-stdin.scm"))
    ("prefix-arguments"
     . ,(local-file "../e2e-tests/tools/prefix-arguments.scm"))
    ("scatter"
     . ,(local-file "../e2e-tests/tools/scatter.scm"))
    ("spell-check"
     . ,(local-file "../e2e-tests/tools/spell-check.scm"))
    ("staging-input-files"
     . ,(local-file "../e2e-tests/tools/staging-input-files.scm"))))

(define e2e-tools
  (file-union "e2e-tools"
              (map (match-lambda
                     ((ccwl-source-filename . ccwl-source-file)
                      (let ((cwl-filename (string-append (basename ccwl-source-filename ".scm")
                                                         ".cwl")))
                        (list cwl-filename
                              (computed-file cwl-filename
                                             (ccwl-compile ccwl-source-file))))))
                   e2e-tools-ccwl-sources)))

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
