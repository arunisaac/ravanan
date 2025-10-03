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

(use-modules (srfi srfi-64)
             (ice-9 filesystem)
             (ice-9 match)
             (web uri)
             (ravanan reader)
             (ravanan work command-line-tool)
             (ravanan work utils))

(define normalize-formal-input
  (@@ (ravanan reader) normalize-formal-input))

(define normalize-formal-output
  (@@ (ravanan reader) normalize-formal-output))

(define normalize-input
  (@@ (ravanan reader) normalize-input))

(test-begin "reader")

(test-equal "Coerce number to number"
  37
  (coerce-type 37 'number))

(test-equal "Normalize File type formal input"
  (canonicalize-json '(("type" . "File")
                       ("id" . "foo")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #t))))))
  (canonicalize-json (normalize-formal-input
                      '(("type" . "File")
                        ("id" . "foo")
                        ("secondaryFiles" . #(".bai"))))))

(test-equal "Normalize File array type formal input"
  (canonicalize-json '(("type"
                        ("type" . "array")
                        ("items" . "File"))
                       ("id" . "foo")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #t))))))
  (canonicalize-json (normalize-formal-input
                      '(("type"
                         ("type" . "array")
                         ("items" . "File"))
                        ("id" . "foo")
                        ("secondaryFiles" . #(".bai"))))))

(test-equal "Normalize array of File arrays type formal input"
  (canonicalize-json '(("type"
                        ("type" . "array")
                        ("items" . (("type" . "array")
                                    ("items" . "File"))))
                       ("id" . "foo")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #t))))))
  (canonicalize-json (normalize-formal-input
                      '(("type"
                         ("type" . "array")
                         ("items" . (("type" . "array")
                                     ("items" . "File"))))
                        ("id" . "foo")
                        ("secondaryFiles" . #(".bai"))))))

(test-equal "Normalize File type formal output"
  (canonicalize-json '(("type" . "File")
                       ("id" . "foo")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #f))))))
  (canonicalize-json (normalize-formal-output
                      '(("type" . "File")
                        ("id" . "foo")
                        ("secondaryFiles" . #(".bai"))))))

(test-equal "Normalize File array type formal output"
  (canonicalize-json '(("type"
                        ("type" . "array")
                        ("items" . "File"))
                       ("id" . "foo")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #f))))))
  (canonicalize-json (normalize-formal-output
                      '(("type"
                         ("type" . "array")
                         ("items" . "File"))
                        ("id" . "foo")
                        ("secondaryFiles" . #(".bai"))))))

(test-equal "Normalize array of File arrays type formal output"
  (canonicalize-json '(("type"
                        ("type" . "array")
                        ("items" . (("type" . "array")
                                    ("items" . "File"))))
                       ("id" . "foo")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #f))))))
  (canonicalize-json (normalize-formal-output
                      '(("type"
                         ("type" . "array")
                         ("items" . (("type" . "array")
                                     ("items" . "File"))))
                        ("id" . "foo")
                        ("secondaryFiles" . #(".bai"))))))

(call-with-temporary-directory
 (lambda (dir)
   (test-equal "Normalize inputs with only location"
     (canonicalize-json
      (let ((path (expand-file-name "foo" dir)))
        `(("class" . "File")
          ("location" . ,(uri->string (build-uri 'file #:path path)))
          ("path" . ,path)
          ("basename" . "foo")
          ("nameroot" . "foo")
          ("nameext" . "")
          ("size" . 0)
          ("checksum" . "sha1$da39a3ee5e6b4b0d3255bfef95601890afd80709"))))
     (canonicalize-json
      (call-with-current-directory dir
        (lambda ()
          ;; Create an actual file called "foo" so that canonicalize-path works.
          (call-with-output-file "foo"
            (const #t))
          (normalize-input '(("class" . "File")
                             ("location" . "foo")))))))))

(call-with-temporary-directory
 (lambda (dir)
   (test-equal "Normalize inputs with only path"
     (canonicalize-json
      (let ((path (expand-file-name "foo" dir)))
        `(("class" . "File")
          ("location" . ,(uri->string (build-uri 'file #:path path)))
          ("path" . ,path)
          ("basename" . "foo")
          ("nameroot" . "foo")
          ("nameext" . "")
          ("size" . 0)
          ("checksum" . "sha1$da39a3ee5e6b4b0d3255bfef95601890afd80709"))))
     (canonicalize-json
      (call-with-current-directory dir
        (lambda ()
          ;; Create an actual file called "foo" so that canonicalize-path
          ;; works.
          (call-with-output-file "foo"
            (const #t))
          (normalize-input '(("class" . "File")
                             ("path" . "foo")))))))))

(test-end "reader")
