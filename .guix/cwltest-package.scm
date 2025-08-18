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

(define-module (cwltest-package)
  #:use-module ((gnu packages bioinformatics)
                #:select (cwltool python-schema-salad))
  #:use-module ((gnu packages check)
                #:select (python-pytest python-pytest-xdist))
  #:use-module ((gnu packages node) #:select (node))
  #:use-module ((gnu packages python-build)
                #:select (python-setuptools python-setuptools-scm python-wheel))
  #:use-module ((gnu packages python-check) #:select (python-junit-xml))
  #:use-module ((gnu packages xml) #:select (python-defusedxml))
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; cwltest uses cwltool as a library. so, create a library version of cwltool
;; where inputs become propagated inputs.
(define python-cwltool
  (package
    (inherit cwltool)
    (name "python-cwltool")
    (inputs
     (list node))
    (propagated-inputs
     (modify-inputs (package-inputs cwltool)
       (delete "node")))))

;; cwltest requires cwl-runner, the implementation-agnostic entry point to
;; cwltool, for its tests.
(define cwl-runner
  (file-union "cwl-runner"
              `(("bin/cwl-runner" ,(file-append cwltool "/bin/cwltool")))))

(define-public cwltest
  (package
    (name "cwltest")
    (version "2.6.20250314152537")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cwltest" version))
       (sha256
        (base32 "0h2w9bllb6cz8d5pja5lbbd1kj08yga40jdi3300anwllczcnfq6"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:modules `((rnrs io ports)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (srfi srfi-171)
                       (guix build pyproject-build-system)
                       (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-docker-in-tests
                 (lambda _
                   ;; Remove DockerRequirement (lines 7–10).
                   (let* ((file "tests/test-data/v1.0/cat1-testcli.cwl")
                          (lines (call-with-input-file file
                                   (cut port-transduce
                                        identity
                                        rcons
                                        get-line
                                        <>))))
                     (call-with-output-file file
                       (lambda (port)
                         (for-each (lambda (line)
                                     (display line port)
                                     (newline port))
                                   (append (take lines 6)
                                           (drop lines 10))))))))
               (add-after 'install 'fix-permissions
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Make test scripts executable.
                   (for-each (lambda (file)
                               (chmod (string-append (site-packages inputs outputs)
                                                     "/cwltest/tests/test-data/"
                                                     file)
                                      #o755))
                             (list "dummy-executor.sh"
                                   "mock_cwl_runner.py")))))))
    (inputs (list python-defusedxml
                  python-junit-xml
                  python-pytest
                  python-schema-salad))
    (native-inputs (list cwl-runner
                         python-cwltool
                         python-pytest
                         python-pytest-xdist
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://github.com/common-workflow-language/cwltest")
    (synopsis "Common Workflow Language testing framework")
    (description "Common Workflow Language testing framework.")
    (license license:asl2.0)))

cwltest
