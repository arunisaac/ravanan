;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2024–2025 Arun Isaac <arunisaac@systemreboot.net>
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
             (guix gexp)
             (ice-9 match)
             (ravanan work monads)
             (ravanan work utils)
             (ravanan javascript))

(define (gexp->sexp-rec exp)
  "Recursively convert G-expression @var{exp} into an approximate S-expression."
  (match exp
    ((? gexp? exp)
     (gexp->sexp-rec (gexp->approximate-sexp exp)))
    ((head tail ...)
     (cons (gexp->sexp-rec head)
           (map gexp->sexp-rec tail)))
    (atom
     atom)))

(test-begin "javascript")

(test-equal "evaluate parameter reference"
  "c"
  (evaluate-javascript-expression "$(inputs.message['bar'][\"foo\"][2])"
                                  '(("inputs" ("message" ("bar" ("foo" . #("a" "b" "c" "d"))))))))

(test-equal "evaluate null parameter reference"
  'null
  (evaluate-javascript-expression "$(null)"
                                  '()))

(test-equal "evaluate parameter reference to JSON object"
  (canonicalize-json '(("class" . "File")
                       ("path" . "/foo/bar")))
  (canonicalize-json
   (evaluate-javascript-expression "$(inputs.fasta)"
                                   '(("inputs" ("fasta"
                                                ("class" . "File")
                                                ("path" . "/foo/bar")))))))

(test-equal "evaluate parameter reference with string interpolation"
  "24foo12foobar"
  (evaluate-javascript-expression "$(runtime.cores)foo$(inputs.threads)$(inputs.output_filename)"
                                  '(("inputs"
                                     ("threads" . 12)
                                     ("output_filename" . "foobar"))
                                    ("runtime" ("cores" . 24)))))

(test-equal "evaluate parameter reference with string interpolation of JSON trees"
  "foo[0,1,2,3]{\"bar\":2,\"foo\":1}"
  (evaluate-javascript-expression "foo$(inputs.vector)$(inputs.object)"
                                  '(("inputs"
                                     ("object"
                                      ("foo" . 1)
                                      ("bar" . 2))
                                     ("vector" . #(0 1 2 3))))))

(test-equal "evaluate parameter reference with node"
  3
  (evaluate-javascript-expression "$(inputs.n + 1)"
                                  '(("inputs" ("n" . 2)))))

(test-equal "evaluate null parameter reference with node"
  ''null
  (gexp->sexp-rec (evaluate-javascript-expression "$(null)")))

(test-equal "evaluate parameter reference to JSON object using node"
  '(json-ref inputs "fasta")
  (gexp->sexp-rec
   (evaluate-javascript-expression "$(inputs.fasta)")))

(test-equal "evaluate parameter reference with string interpolation using node"
  "24foo24foobar"
  (evaluate-javascript-expression "$(runtime.cores)foo$(inputs.threads*2)$(inputs.output_filename)"
                                  '(("inputs"
                                     ("threads" . 12)
                                     ("output_filename" . "foobar"))
                                    ("runtime" ("cores" . 24)))))

(test-equal "evaluate parameter reference with string interpolation of JSON trees using node"
  "foo[0,1,2,3]{\"bar\":2,\"foo\":1}20"
  (evaluate-javascript-expression "foo$(inputs.vector)$(inputs.object)$(inputs.object.foo*20)"
                                  '(("inputs"
                                     ("object"
                                      ("foo" . 1)
                                      ("bar" . 2))
                                     ("vector" . #(0 1 2 3))))))

(test-equal "evaluate parameter reference (without context)"
  '(json-ref inputs "message" "bar" "foo" 2)
  (gexp->sexp-rec
   (evaluate-javascript-expression "$(inputs.message['bar'][\"foo\"][2])")))

(test-equal "evaluate parameter reference with string interpolation (without context)"
  '(string-join
    (map (lambda (value)
           (if (string? value) value (scm->json-string (canonicalize-json value))))
         (list (json-ref runtime "cores")
               "foo"
               (json-ref inputs "threads")
               (json-ref inputs "output_filename")))
    "")
  (gexp->sexp-rec
   (evaluate-javascript-expression "$(runtime.cores)foo$(inputs.threads)$(inputs.output_filename)")))

(test-equal "evaluate parameter reference with string interpolation of JSON trees (without context)"
  '(string-join
    (map (lambda (value)
           (if (string? value) value (scm->json-string (canonicalize-json value))))
         (list "foo" (json-ref inputs "vector") (json-ref inputs "object")))
    "")
  (gexp->sexp-rec
   (evaluate-javascript-expression "foo$(inputs.vector)$(inputs.object)")))

(test-equal "evaluate parameter reference with node (without context)"
  '(evaluate-javascript (*approximate*)
                        "(inputs.n + 1)"
                        (string-append ""
                                       "var inputs = " (scm->json-string inputs) ";"
                                       "var self = " (scm->json-string self) ";"
                                       "var runtime = " (scm->json-string runtime) ";"))
  (gexp->sexp-rec
   (evaluate-javascript-expression "$(inputs.n + 1)")))

(test-equal "evaluate parameter reference with string interpolation using node (without context)"
  '(string-join
    (map (lambda (value)
           (if (string? value) value (scm->json-string (canonicalize-json value))))
         (list (json-ref runtime "cores")
               "foo"
               (evaluate-javascript (*approximate*)
                                    "(inputs.threads*2)"
                                    (string-append ""
                                                   "var inputs = " (scm->json-string inputs) ";"
                                                   "var self = " (scm->json-string self) ";"
                                                   "var runtime = " (scm->json-string runtime) ";"))
               (json-ref inputs "output_filename")))
    "")
  (gexp->sexp-rec
   (evaluate-javascript-expression "$(runtime.cores)foo$(inputs.threads*2)$(inputs.output_filename)")))

(test-equal "evaluate parameter reference with string interpolation of JSON trees using node (without context)"
  '(string-join
    (map (lambda (value)
           (if (string? value) value (scm->json-string (canonicalize-json value))))
         (list "foo"
               (json-ref inputs "vector")
               (json-ref inputs "object")
               (evaluate-javascript (*approximate*)
                                    "(inputs.object.foo*20)"
                                    (string-append ""
                                                   "var inputs = " (scm->json-string inputs) ";"
                                                   "var self = " (scm->json-string self) ";"
                                                   "var runtime = " (scm->json-string runtime) ";"))))
    "")
  (gexp->sexp-rec
   (evaluate-javascript-expression "foo$(inputs.vector)$(inputs.object)$(inputs.object.foo*20)")))

(test-equal "evaluate javascript expression with parentheses"
  0
  (evaluate-javascript-expression "$(1 - (2 - 1))"
                                  '()))

(test-equal "evaluate javascript function body"
  2
  (evaluate-javascript-expression "${switch (inputs.message) { case \"foo\": return 1; case \"bar\": return 2; default: return 3}}"
                                  '(("inputs" . (("message" . "bar")
                                                 ("threads" . 48)))
                                    ("self" . #f)
                                    ("runtime" . #f))))

(test-end "javascript")
