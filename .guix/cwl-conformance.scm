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

(define-module (cwl-conformance)
  #:use-module ((cwltest-package) #:select (cwltest))
  #:use-module ((ravanan-package) #:select (ravanan))
  #:use-module ((gnu packages bioinformatics) #:select (ccwl))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-web) #:select (python-pybadges))
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define* (cwltest-suite-gexp cwltest-suite manifest-file #:key (skip-tests '()))
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match))

        ;; cwltest writes out output directories to TMPDIR, but does not clean
        ;; up after. So, we set TMPDIR to our own temporary directory that we
        ;; can manage easily. See pending issue on cleaning up temporary output
        ;; directories:
        ;; https://github.com/common-workflow-language/cwltest/issues/249
        (mkdir "tmpdir")
        (setenv "TMPDIR" "tmpdir")
        (apply invoke
               #$(file-append cwltest "/bin/cwltest")
               "--test" #$cwltest-suite
               "--tool" #$(file-append ravanan "/bin/ravanan")
               "--badgedir" "badges"
               (append '#$(match skip-tests
                            (() '())
                            (_ (list "-S" (string-join skip-tests ","))))
                       (list "--"
                             "--store=store"
                             (string-append "--guix-manifest=" #$manifest-file)))))))

(define cwl-v1.2-conformance-suite
  (let ((version "1.2.1"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/common-workflow-language/cwl-v1.2")
             (commit (string-append "v" version))))
      (file-name (git-file-name "cwl-v1.2" version))
      (sha256
       (base32
        "03q8pd0niaaff52n6sn07l3rjnvwi4da649lnc8mn928sh0vywf3")))))

(define-public cwl-v1.2-conformance
  (program-file "cwl-v1.2-conformance"
                (cwltest-suite-gexp
                 (file-append cwl-v1.2-conformance-suite
                              "/conformance_tests.yaml")
                 (local-file "../cwl-conformance/manifest.scm")
                 ;; With these tests, evil things happen and too much memory is
                 ;; consumed. So, disable for now.
                 #:skip-tests (list "env_home_tmpdir"
                                    "env_home_tmpdir_docker"
                                    "env_home_tmpdir_docker_no_return_code"))))

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
  `(("hello-world.scm" . ,(local-file "../e2e-tests/tools/hello-world.scm"))))

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

(define generate-badges-gexp
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match))

        (match (command-line)
          ((_ cwltest-badgedir output-directory)
           (set-path-environment-variable
            "GUIX_PYTHONPATH"
            '(#$(string-append "lib/python"
                               (version-major+minor (package-version python))
                               "/site-packages"))
            (list #$(profile
                      (content (packages->manifest
                                (list python python-pybadges))))))
           (invoke #$(file-append python "/bin/python3")
                   #$(local-file "../cwl-conformance/badgegen.py")
                   cwltest-badgedir
                   #$(local-file "../cwl-conformance/commonwl.svg")
                   output-directory))
          ((program _ ...)
           (format (current-error-port)
                   "Usage: ~a CWLTEST_BADGEDIR OUTPUT-DIRECTORY~%"
                   program)
           (exit #f))))))

(define-public generate-badges
  (program-file "generate-badges"
                generate-badges-gexp))

cwl-v1.2-conformance
