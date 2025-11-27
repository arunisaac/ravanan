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
             (ravanan work utils)
             (ravanan work vectors))

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

(test-equal "Normalize inputs with only location"
  (canonicalize-json
   (let ((path (canonicalize-path "test-data/foo")))
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
  (call-with-values
      (cut read-workflow+inputs
           "test-data/workflow-with-a-file-input.cwl"
           "test-data/input-file-with-location-only.yaml")
    (lambda (workflow inputs)
      (canonicalize-json (assoc-ref inputs "foo")))))

(test-equal "Normalize inputs with only path"
  (canonicalize-json
   (let ((path (canonicalize-path "test-data/foo")))
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
  (call-with-values
      (cut read-workflow+inputs
           "test-data/workflow-with-a-file-input.cwl"
           "test-data/input-file-with-path-only.yaml")
    (lambda (workflow inputs)
      (canonicalize-json (assoc-ref inputs "foo")))))

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

(test-equal "Resolve type ambiguities in workflow default inputs"
  '(("number" . 13)
    ("flag" . #t)
    ("reverseflag" . #f)
    ("foo" . "bar")
    ("arr" . #(1 2 3)))
  (call-with-values
      (cut read-workflow+inputs
           "test-data/workflow-with-default-inputs.cwl"
           "test-data/empty.yaml")
    (lambda (workflow inputs)
      (vector-map->list (lambda (input)
                          (cons (assoc-ref input "id")
                                (assoc-ref input "default")))
                        (assoc-ref workflow "inputs")))))

(test-equal "Normalize File type formals"
  (list (vector-map canonicalize-json
                    #((("id" . "infoo")
                       ("type" . "File")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #t)))))
                      (("id" . "inbar")
                       ("type"
                        ("type" . "array")
                        ("items" . "File"))
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #t)))))
                      (("id" . "infoobar")
                       ("type"
                        ("type" . "array")
                        ("items" . (("type" . "array")
                                    ("items" . "File"))))
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #t)))))))
        (vector-map canonicalize-json
                    #((("id" . "outfoo")
                       ("type" . "File")
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #f)))))
                      (("id" . "outbar")
                       ("type"
                        ("type" . "array")
                        ("items" . "File"))
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #f)))))
                      (("id" . "outfoobar")
                       ("type"
                        ("type" . "array")
                        ("items" . (("type" . "array")
                                    ("items" . "File"))))
                       ("secondaryFiles" . #((("pattern" . ".bai")
                                              ("required" . #f))))))))
  (call-with-values
      (cut read-workflow+inputs
           "test-data/workflow-with-various-file-type-formals.cwl"
           "test-data/empty.yaml")
    (lambda (workflow inputs)
      (list (vector-map canonicalize-json
                        (assoc-ref workflow "inputs"))
            (vector-map canonicalize-json
                        (assoc-ref workflow "outputs"))))))

(test-end "reader")
