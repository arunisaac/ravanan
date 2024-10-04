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
;; a full-blown javascript engine.

;;; Code:

(define-module (ravanan javascript)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:export (evaluate-simple-parameter-reference
            tokenize-parameter-references))

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

(define* (evaluate-parameter-reference-1 expression context)
  "Compile parameter reference @var{expression} to a G-expression that references
the variables @code{inputs}, @code{self} or @code{runtime}. @var{expression}
must strictly be a single parameter reference and will not be subject to string
interpolation.

If @var{context} is not @code{#f}, evaluate the parameter reference in that
context and return the value. @var{context} must be an association list with
keys @code{\"inputs\"}, @code{\"self\"} and @code{\"runtime\"}.

The returned value is maybe-monadic. If @var{expression} fails to parse,
@code{Nothing} is returned."
  (match (peg:tree (match-pattern parameter-reference expression))
    ;; Special case for null
    (('parameter-reference "null")
     (just (if context
               'null
               #~'null)))
    ;; Disallow referencing anything other than inputs, self or runtime.
    (('parameter-reference (and (not (or "inputs" "self" "runtime"))
                                symbol)
                           _ ...)
     (user-error "Invalid parameter reference; `~a' unknown"
                 symbol))
    ;; Parse parameter reference and produce a G-expression. The strange and
    ;; complex matching pattern for segments accounts for how (ice-9 peg) adds
    ;; an additional layer of parentheses to the tree when there are 2 or more
    ;; segments.
    (('parameter-reference symbol . (or (('segment segments) ...)
                                        ((('segment segments) ...))))
     (let ((segments (map (match-lambda
                            (('index key) (string->number key))
                            (key key))
                          segments)))
       (just (if context
                 (apply json-ref context symbol segments)
                 #~(json-ref #$symbol #$@segments)))))
    ;; Perhaps this is a more complex javascript expression.
    (#f %nothing)))

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

(define* (evaluate-simple-parameter-reference str #:optional context)
  "Compile parameter reference @var{str} to a G-expression that references
the variables @code{inputs}, @code{self} or @code{runtime}. @var{str} may be
subject to string interpolation.

If @var{context} is not @code{#f}, evaluate the parameter reference in that
context and return the value. @var{context} must be an association list with
keys @code{\"inputs\"}, @code{\"self\"} and @code{\"runtime\"}.

The returned value is maybe-monadic. If any of the parameter references in
@var{str} fail to parse, @code{Nothing} is returned."
  (define (evaluate-token token)
    (if (and (string-prefix? "$(" token)
             (string-suffix? ")" token))
        ;; Drop the leading "$" and evaluate.
        (evaluate-simple-parameter-reference-1 (string-drop token 1)
                                               context)
        ;; token is a string literal.
        (just token)))

  (maybe-let* ((evaluated-tokens
                (fold (lambda (token maybe-result)
                        (maybe-let* ((result maybe-result))
                          (maybe-let* ((evaluated-token (evaluate-token token)))
                            (just (cons evaluated-token result)))))
                      (just (list))
                      (tokenize-parameter-references str))))
    (just (if context
              ;; Evaluate immediately.
              (string-join (map (lambda (token)
                                  (if (string? token)
                                      token
                                      (scm->json-string (canonicalize-json token))))
                                (reverse evaluated-tokens))
                           "")
              ;; Compile to a G-expression that interpolates parameter reference
              ;; string.
              #~(string-join (map (lambda (token)
                                    (if (string? token)
                                        token
                                        (scm->json-string (canonicalize-json token))))
                                  (list #$@(reverse evaluated-tokens)))
                             "")))))
