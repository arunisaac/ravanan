;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
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
             (ravanan work monads)
             (ravanan javascript))

(test-begin "javascript")

(test-equal "evaluate parameter reference"
  "c"
  (evaluate-parameter-reference "$(inputs.message['bar'][\"foo\"][2])"
                                '(("inputs" ("message" ("bar" ("foo" . #("a" "b" "c" "d"))))))))

(test-equal "evaluate parameter reference with string interpolation"
  "24foo12foobar"
  (evaluate-parameter-reference "$(runtime.cores)foo$(inputs.threads)$(inputs.output_filename)"
                                '(("inputs"
                                   ("threads" . 12)
                                   ("output_filename" . "foobar"))
                                  ("runtime" ("cores" . 24)))))

(test-equal "evaluate parameter reference with string interpolation of JSON trees"
  "foo[0,1,2,3]{\"bar\":2,\"foo\":1}"
  (evaluate-parameter-reference "foo$(inputs.vector)$(inputs.object)"
                                '(("inputs"
                                   ("object"
                                    ("foo" . 1)
                                    ("bar" . 2))
                                   ("vector" . #(0 1 2 3))))))

(test-equal "evaluate parameter reference with node"
  "3"
  (evaluate-parameter-reference "$(inputs.n + 1)"
                                '(("inputs" ("n" . 2)))))

(test-equal "evaluate parameter reference with string interpolation using node"
  "24foo24foobar"
  (evaluate-parameter-reference "$(runtime.cores)foo$(inputs.threads*2)$(inputs.output_filename)"
                                '(("inputs"
                                   ("threads" . 12)
                                   ("output_filename" . "foobar"))
                                  ("runtime" ("cores" . 24)))))

(test-equal "evaluate parameter reference with string interpolation of JSON trees using node"
  "foo[0,1,2,3]{\"bar\":2,\"foo\":1}20"
  (evaluate-parameter-reference "foo$(inputs.vector)$(inputs.object)$(inputs.object.foo*20)"
                                '(("inputs"
                                   ("object"
                                    ("foo" . 1)
                                    ("bar" . 2))
                                   ("vector" . #(0 1 2 3))))))

(test-end "javascript")
