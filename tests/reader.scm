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

(use-modules (srfi srfi-1)
             (srfi srfi-64)
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

(define (json=? tree1 tree2)
  (cond
   ;; Arrays
   ((vector? tree1)
    (lset= json=?
           (vector->list tree1)
           (vector->list tree2)))
   ;; Dictionaries
   ((list? tree1)
    (lset= (match-lambda*
             (((key1 . value1) (key2 . value2))
              (and (string=? key1 key2)
                   (json=? value1 value2))))
           tree1
           tree2))
   ;; Atoms
   (else
    (equal? tree1 tree2))))

(test-begin "reader")

(test-equal "Coerce number to number"
  37
  (coerce-type 37 'number))

(test-assert "Normalize File type formal input"
  (json=? '(("type" . "File")
            ("id" . "foo")
            ("secondaryFiles" . #((("pattern" . ".bai")
                                   ("required" . #t)))))
          (normalize-formal-input
           '(("type" . "File")
             ("id" . "foo")
             ("secondaryFiles" . #(".bai"))))))

(test-assert "Normalize File array type formal input"
  (json=? '(("type"
             ("type" . "array")
             ("items" . "File"))
            ("id" . "foo")
            ("secondaryFiles" . #((("pattern" . ".bai")
                                   ("required" . #t)))))
          (normalize-formal-input
           '(("type"
              ("type" . "array")
              ("items" . "File"))
             ("id" . "foo")
             ("secondaryFiles" . #(".bai"))))))

(test-assert "Normalize array of File arrays type formal input"
  (json=? '(("type"
             ("type" . "array")
             ("items" . (("type" . "array")
                         ("items" . "File"))))
            ("id" . "foo")
            ("secondaryFiles" . #((("pattern" . ".bai")
                                   ("required" . #t)))))
          (normalize-formal-input
           '(("type"
              ("type" . "array")
              ("items" . (("type" . "array")
                          ("items" . "File"))))
             ("id" . "foo")
             ("secondaryFiles" . #(".bai"))))))

(test-assert "Normalize File type formal output"
  (json=? '(("type" . "File")
            ("id" . "foo")
            ("secondaryFiles" . #((("pattern" . ".bai")
                                   ("required" . #f)))))
          (normalize-formal-output
           '(("type" . "File")
             ("id" . "foo")
             ("secondaryFiles" . #(".bai"))))))

(test-assert "Normalize File array type formal output"
  (json=? '(("type"
             ("type" . "array")
             ("items" . "File"))
            ("id" . "foo")
            ("secondaryFiles" . #((("pattern" . ".bai")
                                   ("required" . #f)))))
          (normalize-formal-output
           '(("type"
              ("type" . "array")
              ("items" . "File"))
             ("id" . "foo")
             ("secondaryFiles" . #(".bai"))))))

(test-assert "Normalize array of File arrays type formal output"
  (json=? '(("type"
             ("type" . "array")
             ("items" . (("type" . "array")
                         ("items" . "File"))))
            ("id" . "foo")
            ("secondaryFiles" . #((("pattern" . ".bai")
                                   ("required" . #t)))))
          (normalize-formal-input
           '(("type"
              ("type" . "array")
              ("items" . (("type" . "array")
                          ("items" . "File"))))
             ("id" . "foo")
             ("secondaryFiles" . #(".bai"))))))

(test-assert "Normalize inputs with only location"
  (call-with-temporary-directory
   (lambda (dir)
     (json=? (let ((path (expand-file-name "foo" dir)))
               `(("class" . "File")
                 ("location" . ,(uri->string (build-uri 'file #:path path)))
                 ("path" . ,path)
                 ("basename" . "foo")
                 ("nameroot" . "foo")
                 ("nameext" . "")
                 ("size" . 0)
                 ("checksum" . "sha1$da39a3ee5e6b4b0d3255bfef95601890afd80709")))
             (call-with-current-directory dir
               (lambda ()
                 ;; Create an actual file called "foo" so that canonicalize-path
                 ;; works.
                 (call-with-output-file "foo"
                   (const #t))
                 (normalize-input '(("class" . "File")
                                    ("location" . "foo")))))))))

(test-assert "Normalize inputs with only path"
  (call-with-temporary-directory
   (lambda (dir)
     (json=? (let ((path (expand-file-name "foo" dir)))
               `(("class" . "File")
                 ("location" . ,(uri->string (build-uri 'file #:path path)))
                 ("path" . ,path)
                 ("basename" . "foo")
                 ("nameroot" . "foo")
                 ("nameext" . "")
                 ("size" . 0)
                 ("checksum" . "sha1$da39a3ee5e6b4b0d3255bfef95601890afd80709")))
             (call-with-current-directory dir
               (lambda ()
                 ;; Create an actual file called "foo" so that canonicalize-path
                 ;; works.
                 (call-with-output-file "foo"
                   (const #t))
                 (normalize-input '(("class" . "File")
                                    ("path" . "foo")))))))))

(test-end "reader")
