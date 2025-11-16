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
  #:use-module ((gnu packages guile) #:select (guile-3.0))
  #:use-module ((gnu packages scheme) #:select (chibi-scheme))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public guile-chibi-match
  (package
    (name "guile-chibi-match")
    (version (package-version chibi-scheme))
    (source (package-source chibi-scheme))
    (build-system guile-build-system)
    (arguments
     (list #:modules '((guix build guile-build-system)
                       (guix build utils)
                       (ice-9 ftw))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'rearrange-files
                 (lambda _
                   ;; Delete and rearrange files so that (chibi match)
                   ;; is in the expected directory hierarchy.
                   (chdir "lib/chibi")
                   (rename-file "match" "chibi")
                   (for-each (lambda (file)
                               (unless (member file (list "." ".." "chibi"))
                                 (delete-file-recursively file)))
                             (scandir "."))
                   (substitute* "chibi/match.scm"
                     ((";; 2006/12/01[^\n]*" line)
                      (string-append line
                                     "\n(define-module (chibi match)
#:export (match match-lambda match-lambda* match-let match-letrec match-let*))"))))))))
    (inputs
     (list guile-3.0))
    (home-page "https://synthcode.com/scheme/chibi/lib/chibi/match.html")
    (synopsis "Portable hygienic pattern matcher")
    (description "@code{guile-chibi-match} is a portable hygienic
pattern matcher for Scheme.  This is a full superset of the popular
match package by Andrew Wright, written in fully portable syntax-rules
and thus preserving hygiene.  The most notable extensions are the
ability to use non-linear patterns---patterns in which the same
identifier occurs multiple times, tail patterns after ellipsis, and
the experimental tree patterns.")
    (license (package-license chibi-scheme))))

(define-public run64
  (let ((commit "79976781fd609409cd5ac543b3ecfa4d2531dd6a")
        (revision "0"))
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
                  "1fxz5463hv609hqmqn3rph3z9fd4viqxfxxx2js1y75zqqyb1ab9"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~(list (string-append "prefix=" #$output))
             #:modules `(((guix build guile-build-system)
                          #:select (target-guile-effective-version))
                         ,@%default-gnu-imported-modules)
             #:phases
             (with-imported-modules `((guix build guile-build-system)
                                      ,@%default-gnu-imported-modules)
               #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-after 'install 'wrap
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out"))
                             (effective-version (target-guile-effective-version)))
                         (wrap-program (string-append out "/bin/guile-run64")
                           `("GUILE_LOAD_PATH" prefix
                             (,(string-append out "/share/guile/site/" effective-version)
                              ,(getenv "GUILE_LOAD_PATH")))
                           `("GUILE_LOAD_COMPILED_PATH" prefix
                             (,(string-append out "/lib/guile/"
                                              effective-version "/site-ccache")
                              ,(getenv "GUILE_LOAD_COMPILED_PATH")))))))))))
      (inputs
       (list guile-3.0
             guile-chibi-match))
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
       (prepend run64)))))

ravanan
