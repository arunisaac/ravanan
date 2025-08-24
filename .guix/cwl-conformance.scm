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
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-web) #:select (python-pybadges))
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils))

(define* (cwltest-suite-gexp manifest-file #:key (skip-tests '()))
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match))

        (match (command-line)
          ((_ cwltest-suite)
           ;; cwltest writes out output directories to TMPDIR, but does not
           ;; clean up after. So, we set TMPDIR to our own temporary directory
           ;; that we can manage easily. See pending issue on cleaning up
           ;; temporary output directories:
           ;; https://github.com/common-workflow-language/cwltest/issues/249
           (mkdir "tmpdir")
           (setenv "TMPDIR" "tmpdir")
           (invoke #$(file-append cwltest "/bin/cwltest")
                   "--test" cwltest-suite
                   ;; With these tests, evil things happen and too much
                   ;; memory is consumed. So, disable for now.
                   "-S" (string-join skip-tests ",")
                   "--tool" #$(file-append ravanan "/bin/ravanan")
                   "--badgedir" "badges"
                   "--"
                   "--store=store"
                   (string-append "--guix-manifest=" #$manifest-file)))
          ((program _ ...)
           (format (current-error-port)
                   "Usage: ~a CWLTEST-SUITE~%"
                   program)
           (exit #f))))))

(define-public cwl-v1.2-conformance
  (program-file "cwl-v1.2-conformance"
                (cwltest-suite-gexp
                 (local-file "../cwl-conformance/manifest.scm")
                 #:skip-tests (list "env_home_tmpdir"
                                    "env_home_tmpdir_docker"
                                    "env_home_tmpdir_docker_no_return_code"))))

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
