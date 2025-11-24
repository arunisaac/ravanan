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

(define-module (ravanan-website)
  #:use-module ((gnu packages fonts) #:select (font-charter font-fira-code))
  #:use-module ((gnu packages haskell-xyz) #:select (pandoc))
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((ravanan-package) #:select (ravanan)))

(define ravanan-website-home-page-gexp
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (copy-file #$(file-append (package-source ravanan)
                                  "/README.md")
                   "README.md")
        ;; Add Download section.
        (substitute* "README.md"
          (("^- \\[Building from source\\]\\(#building-from-source\\)" all)
           (string-append "- [Download](#download)\n"
                          all))
          (("^# Building from source" all)
           (string-append "# Download

Download release tarballs.

- 2025-11-24 [ravanan-0.2.0.tar.lz](releases/ravanan-0.2.0.tar.lz) [GPG Signature](releases/ravanan-0.2.0.tar.lz.asc)
- 2025-01-28 [ravanan-0.1.0.tar.lz](releases/ravanan-0.1.0.tar.lz) [GPG Signature](releases/ravanan-0.1.0.tar.lz.asc)

Download [public signing key](https://systemreboot.net/about/arunisaac.pub).

Browse the development version on [cgit](https://git.systemreboot.net/ravanan) or on [GitHub](https://github.com/arunisaac/ravanan/).
"
                          all)))
        (invoke #$(file-append pandoc "/bin/pandoc")
                "--standalone"
                "--metadata" "title=ravanan"
                "--metadata" "document-css=false"
                "--css=style.css"
                "--from=gfm"
                (string-append "--output=" #$output)
                "README.md"))))

(define-public ravanan-website
  (file-union "ravanan-website"
              `(("index.html"
                 ,(computed-file "ravanan-website-home-page.html"
                                 ravanan-website-home-page-gexp))
                ("images/ravanan-king-of-lanka.jpg"
                 ,(local-file "../images/ravanan-king-of-lanka.jpg"))
                ("style.css" ,(local-file "../website/style.css"))
                ("fonts/charter_regular.woff2"
                 ,(file-append font-charter
                               "/share/fonts/web/charter_regular.woff2"))
                ("fonts/FiraCode-Regular.woff2"
                 ,(file-append font-fira-code
                               "/share/fonts/web/FiraCode-Regular.woff2"))
                ("fonts/FiraCode-SemiBold.woff2"
                 ,(file-append font-fira-code
                               "/share/fonts/web/FiraCode-SemiBold.woff2")))))

ravanan-website
