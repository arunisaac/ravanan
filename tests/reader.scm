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

(use-modules (srfi srfi-26)
             (srfi srfi-64)
             (ice-9 filesystem)
             (ice-9 match)
             (web uri)
             (ravanan reader)
             (ravanan work command-line-tool)
             (ravanan work types)
             (ravanan work utils))

(define normalize-formal-input
  (@@ (ravanan reader) normalize-formal-input))

(define normalize-formal-output
  (@@ (ravanan reader) normalize-formal-output))

(define normalize-input
  (@@ (ravanan reader) normalize-input))

(test-begin "reader")

(test-equal "Coerce to boolean (true)"
  #t
  (coerce-type "true" 'boolean))

(test-equal "Coerce to boolean (false)"
  #f
  (coerce-type "false" 'boolean))

(test-equal "Coerce to int"
  37
  (coerce-type "37" 'int))

(test-equal "Coerce to float"
  37.1
  (coerce-type "37.1" 'float))

(test-equal "Coerce to double"
  37.1
  (coerce-type "37.1" 'double))

(test-equal "Coerce to string"
  "37"
  (coerce-type "37" 'string))

(test-equal "Coerce to File"
  '(("class" . "File")
    ("location" . "foo"))
  (coerce-type '(("class" . "File")
                 ("location" . "foo"))
               'File))

(test-equal "Coerce to array"
  #(1 2 3)
  (coerce-type #("1" "2" "3")
               (cwl-array-type 'int)))

(test-equal "Coerce to union type (int first)"
  37
  (coerce-type "37"
               (cwl-union-type 'int 'string)))

(test-equal "Coerce to union type (string first)"
  "37"
  (coerce-type "37"
               (cwl-union-type 'string 'int)))

(test-equal "Coerce int to int"
  37
  (coerce-type 37 'int))

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
          ("location" . ,(uri->string (build-uri 'file
                                                 #:host ""
                                                 #:path path
                                                 #:validate? #f)))
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
          ("location" . ,(uri->string (build-uri 'file
                                                 #:host ""
                                                 #:path path
                                                 #:validate? #f)))
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

(test-equal "Read YAML inputs file with type ambiguities"
  '(("number" . 13)
    ("flag" . #t)
    ("reverseflag" . #f)
    ("foo" . "bar")
    ("arr" . #(1 2 3)))
  (call-with-values
      (cut read-workflow+inputs
           "test-data/workflow-for-inputs-with-type-ambiguities.cwl"
           "test-data/inputs-with-type-ambiguities.yaml")
    (lambda (workflow inputs)
      inputs)))

(test-end "reader")
