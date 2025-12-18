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

;;; Commentary:

;; CWL allows parameter references that use a subset of Javascript/ECMAScript
;; 5.1 syntax. This module implements that subset in scheme without resorting to
;; a full-blown javascript engine. In addition, it provides a fallback to the
;; node javascript engine for more complex expressions.

;;; Code:

(define-module (ravanan javascript)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module ((gnu packages node) #:select (node))
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (ravanan config)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:export (javascript-expression?
            evaluate-javascript-expression))

;; node executable for evaluating javascript on worker nodes
(define %worker-node
  (file-append node "/bin/node"))

(define-peg-pattern symbol body
  (+ (or (range #\a #\z)
         (range #\A #\Z)
         (range #\0 #\9)
         "_")))

(define-peg-pattern singleq body
  (and (ignore "['")
       (* (and (not-followed-by (or "|" "\\" "'" "}"))
               peg-any))
       (ignore "']")))

(define-peg-pattern doubleq body
  (and (ignore "[\"")
       (* (and (not-followed-by (or "|" "\\" "\"" "}"))
               peg-any))
       (ignore "\"]")))

(define-peg-pattern index all
  (and (ignore "[")
       (+ (range #\0 #\9))
       (ignore "]")))

(define-peg-pattern segment all
  (and (or (and (ignore ".") symbol) singleq doubleq index)))

(define-peg-pattern parameter-reference all
  (and (ignore "$(")
       (or "inputs" "self" "runtime")
       (* segment) (ignore ")")))

(define-peg-pattern javascript-subexpression body
  (and "("
       (* (or (and (not-followed-by (or "(" ")"))
                   peg-any)
              javascript-subexpression))
       ")"))

(define-peg-pattern javascript-expression all
  (and (ignore "$") javascript-subexpression))

(define-peg-pattern javascript-function-body-subexpression body
  (and "{"
       (* (or (and (not-followed-by (or "{" "}"))
                   peg-any)
              javascript-function-body-subexpression))
       "}"))

(define-peg-pattern javascript-function-body all
  (and (ignore "$") javascript-function-body-subexpression))

(define-peg-pattern whitespace body
  (or "\t" "\n" "\r" " "))

(define-peg-pattern string-literal body
  (+ (and (not-followed-by (or "$(" whitespace))
          peg-any)))

(define-peg-pattern javascript all
  (and (ignore (* whitespace))
       (* (and (* whitespace)
               (or parameter-reference
                   javascript-expression
                   javascript-function-body
                   string-literal)))
       (ignore (* whitespace))))

(define (javascript-expression? str)
  "Return true value if @var{str} contains inline javascript or parameter
references. Return @code{#f} otherwise."
  (match-pattern javascript str))

(define* (evaluate-expression-tree-1 expression-tree context expression-lib)
  "Compile javascript @var{expression-tree} to a G-expression that evaluates
it. The returned G-expression may reference the variables @code{inputs},
@code{self} or @code{runtime}. @var{expression} must strictly be a single
javascript expression and will not be subject to string interpolation.

If @var{context} is not @code{#f}, evaluate the javascript expression in that
context and return the value. @var{context} must be an association list with
keys @code{\"inputs\"}, @code{\"self\"} and @code{\"runtime\"}.

@var{expression-lib} is a list of expressions evaluated before evaluating
@var{expression}."
  (match expression-tree
    ;; String literal
    ((? string? str)
     str)
    ;; Parse parameter reference. The strange and complex matching pattern for
    ;; segments accounts for how (ice-9 peg) adds an additional layer of
    ;; parentheses to the tree when there are 2 or more segments.
    (('parameter-reference symbol . (or (('segment segments) ...)
                                        ((('segment segments) ...))))
     (let ((segments (map (match-lambda
                            (('index key) (string->number key))
                            (key key))
                          segments)))
       (match expression-lib
         (()
          (if context
              ;; Evaluate immediately.
              (apply json-ref context symbol segments)
              ;; Compile to a G-expression that evaluates expression.
              #~(json-ref #$(string->symbol symbol) #$@segments)))
         (_
          (evaluate-using-node (apply string-append
                                      symbol
                                      (map (lambda (segment)
                                             (if (string? segment)
                                                 (string-append "." segment)
                                                 (string-append "[" (number->string segment) "]")))
                                           segments))
                               context
                               expression-lib)))))
    ;; Quick short circuiting for null, true and false
    (('javascript-expression "(null)")
     (if context 'null #~'null))
    (('javascript-expression "(true)")
     (if context #t #~#t))
    (('javascript-expression "(false)")
     (if context #f #~#f))
    ;; This is a more complex javascript expression. Fall back to node.
    (('javascript-expression expression)
     (evaluate-using-node expression context expression-lib))
    ;; This is a javascript function body. Fall back to node.
    (('javascript-function-body function-body)
     (evaluate-using-node (string-append "(function() {" function-body "})()")
                          context
                          expression-lib))))

(define (evaluate-using-node expression context expression-lib)
  "Evaluate javascript @var{expression} using the node javascript engine in
@var{context} with @var{expression-lib}.

@var{context} and @var{expression-lib} are the same as in
@code{evaluate-javascript-expression}."
  (define (context-value name)
    (scm->json-string (assoc-ref context name)))

  (if context
      ;; Evaluate immediately.
      (evaluate-javascript %node
                           expression
                           (string-append (string-join expression-lib)
                                          "var inputs = " (context-value "inputs") ";"
                                          "var self = " (context-value "self") ";"
                                          "var runtime = " (context-value "runtime") ";"))
      ;; Compile to a G-expression that evaluates expression.
      #~(evaluate-javascript #$%worker-node
                             #$expression
                             ;; Context variables are only fully available at
                             ;; runtime. So, defer their reference to the
                             ;; G-expression.
                             (string-append #$(string-join expression-lib)
                                            "var inputs = " (scm->json-string inputs) ";"
                                            "var self = " (scm->json-string self) ";"
                                            "var runtime = " (scm->json-string runtime) ";"))))

(define* (evaluate-javascript-expression str #:optional context (expression-lib '()))
  "Compile javascript expression @var{str} to a G-expression that references
the variables @code{inputs}, @code{self} or @code{runtime}. @var{str} may be
subject to string interpolation.

If @var{context} is not @code{#f}, evaluate the javascript expression in that
context and return the value. @var{context} must be an association list with
keys @code{\"inputs\"}, @code{\"self\"} and @code{\"runtime\"}.

@var{expression-lib} is a list of expressions evaluated before evaluating
@var{expression}."
  (match (peg:tree (match-pattern javascript str))
    ;; There is only one expression. This is not a string interpolation. Do not
    ;; serialize JSON.
    (('javascript expression-tree)
     (evaluate-expression-tree-1 expression-tree
                                 context
                                 expression-lib))
    ;; This is a string interpolation. Evaluate expressions and serialize JSON.
    (('javascript expression-trees ...)
     (let ((vals (map (cut evaluate-expression-tree-1 <> context expression-lib)
                      expression-trees)))
       (if context
           ;; Evaluate immediately.
           (string-join (map (lambda (value)
                               (if (string? value)
                                   value
                                   (scm->json-string (canonicalize-json value))))
                             vals)
                        "")
           ;; Compile to a G-expression that interpolates the javascript
           ;; expression string.
           #~(string-join (map (lambda (value)
                                 (if (string? value)
                                     value
                                     (scm->json-string (canonicalize-json value))))
                               (list #$@vals))
                          ""))))))
