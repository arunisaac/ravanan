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

;;; Commentary:

;; CWL allows parameter references that use a subset of Javascript/ECMAScript
;; 5.1 syntax. This module implements that subset in scheme without resorting to
;; a full-blown javascript engine. In addition, it provides a fallback to the
;; node javascript engine for more complex expressions.

;;; Code:

(define-module (ravanan javascript)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module ((gnu packages node) #:select (node))
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (ravanan config)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:export (evaluate-parameter-reference))

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
  (and (ignore "(") symbol (* segment) (ignore ")")))

(define* (evaluate-parameter-reference-1 expression context expression-lib)
  "Compile parameter reference @var{expression} to a G-expression that evaluates
it. The returned G-expression may reference the variables @code{inputs},
@code{self} or @code{runtime}. @var{expression} must strictly be a single
parameter reference and will not be subject to string interpolation.

If @var{context} is not @code{#f}, evaluate the parameter reference in that
context and return the value. @var{context} must be an association list with
keys @code{\"inputs\"}, @code{\"self\"} and @code{\"runtime\"}.

@var{expression-lib} is a list of expressions evaluated before evaluating
@var{expression}."
  (match expression-lib
    (()
     (match (peg:tree (match-pattern parameter-reference expression))
       ;; Special case for null
       (('parameter-reference "null")
        (if context
            'null
            #~'null))
       ;; Disallow referencing anything other than inputs, self or runtime.
       (('parameter-reference (and (not (or "inputs" "self" "runtime"))
                                   symbol)
                              _ ...)
        (user-error "Invalid parameter reference; `~a' unknown"
                    symbol))
       ;; Parse parameter reference and produce a G-expression. The strange and
       ;; complex matching pattern for segments accounts for how (ice-9 peg)
       ;; adds an additional layer of parentheses to the tree when there are 2
       ;; or more segments.
       (('parameter-reference symbol . (or (('segment segments) ...)
                                           ((('segment segments) ...))))
        (let ((segments (map (match-lambda
                               (('index key) (string->number key))
                               (key key))
                             segments)))
          (if context
              ;; Evaluate immediately.
              (apply json-ref context symbol segments)
              ;; Compile to a G-expression that evaluates expression.
              #~(json-ref #$(string->symbol symbol) #$@segments))))
       ;; Perhaps this is a more complex javascript expression.
       (#f
        (evaluate-using-node expression context expression-lib))))
    ;; expression-lib has been provided. Fall back to node.
    (_
     (evaluate-using-node expression context expression-lib))))

(define (evaluate-using-node expression context expression-lib)
  "This function is the same as @code{evaluate-parameter-reference-1} but uses
the node javascript engine."
  (define (set-variable name value)
    (string-append name " = " (scm->json-string value) ";"))

  (define preamble
    (string-join (append expression-lib
                         (filter-map (match-lambda
                                       (((and (or "inputs" "self" "runtime")
                                              name)
                                         . value)
                                        (set-variable name value))
                                       (_ #f))
                                     context))))

  (if context
      ;; Evaluate immediately.
      (evaluate-javascript %node expression preamble)
      ;; Compile to a G-expression that evaluates expression.
      #~(evaluate-javascript #$%worker-node #$expression #$preamble)))

(define (tokenize-parameter-references str)
  "Split @var{str} into tokens of parameter reference and literal strings."
  (let ((end (if (string-prefix? "$(" str)
                 (1+ (string-index str #\)))
                 (string-index str #\$))))
    (if end
        (cons (substring str 0 end)
              (tokenize-parameter-references (substring str end)))
        (if (string-null? str)
            (list)
            (list str)))))

(define* (evaluate-parameter-reference str #:optional context (expression-lib '()))
  "Compile parameter reference @var{str} to a G-expression that references
the variables @code{inputs}, @code{self} or @code{runtime}. @var{str} may be
subject to string interpolation.

If @var{context} is not @code{#f}, evaluate the parameter reference in that
context and return the value. @var{context} must be an association list with
keys @code{\"inputs\"}, @code{\"self\"} and @code{\"runtime\"}.

@var{expression-lib} is a list of expressions evaluated before evaluating
@var{expression}."
  (define (evaluate-token token)
    (if (and (string-prefix? "$(" token)
             (string-suffix? ")" token))
        ;; Drop the leading "$" and evaluate.
        (evaluate-parameter-reference-1 (string-drop token 1)
                                        context
                                        expression-lib)
        ;; token is a string literal.
        token))

  (let ((evaluated-tokens (map evaluate-token
                               (tokenize-parameter-references str))))
    (if context
        ;; Evaluate immediately.
        (string-join (map (lambda (token)
                            (if (string? token)
                                token
                                (scm->json-string (canonicalize-json token))))
                          evaluated-tokens)
                     "")
        ;; Compile to a G-expression that interpolates parameter reference
        ;; string.
        #~(string-join (map (lambda (token)
                              (if (string? token)
                                  token
                                  (scm->json-string (canonicalize-json token))))
                            (list #$@evaluated-tokens))
                       ""))))
