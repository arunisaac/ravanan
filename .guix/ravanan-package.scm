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
  #:use-module ((gnu packages package-management) #:select (guix))
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages guile) #:select (guile-json-4 guile-next))
  #:use-module ((gnu packages guile-xyz) #:select (guile-filesystem guile-libyaml))
  #:use-module ((gnu packages node) #:select (node))
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils))

(define-public ravanan
  (package
    (name "ravanan")
    (version "0.1.0")
    (source (local-file ".."
                        "ravanan-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (arguments
     (list #:make-flags
           #~(list (string-append "prefix=" #$output)
                   (string-append "NODE=" (search-input-file %build-inputs "bin/node")))
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%gnu-build-system-modules)
           #:phases
           (with-imported-modules `((guix build guile-build-system)
                                    ,@%gnu-build-system-modules)
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-after 'install 'wrap
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (effective-version (target-guile-effective-version)))
                       (wrap-program (string-append out "/bin/ravanan")
                         `("GUILE_LOAD_PATH" prefix
                           (,(string-append out "/share/guile/site/" effective-version)
                            ,(getenv "GUILE_LOAD_PATH")))
                         `("GUILE_LOAD_COMPILED_PATH" prefix
                           (,(string-append out "/lib/guile/" effective-version "/site-ccache")
                            ,(getenv "GUILE_LOAD_COMPILED_PATH")))))))))))
    (inputs
     (list node
           guile-next
           guile-filesystem
           guile-gcrypt
           guile-json-4
           guile-libyaml
           guix))
    (build-system gnu-build-system)
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license license:gpl3+)))

ravanan
