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

(define-module (ravanan-release)
  #:use-module ((gnu packages base) #:select (gnu-make))
  #:use-module ((gnu packages compression) #:select (lzip))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (guix gexp))

(define ravanan-git-repo
  (local-file "../.git"
              "ravanan-git-repo"
              #:recursive? #t))

(define ravanan-release-gexp
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match)
                     (ice-9 popen)
                     (srfi srfi-26)
                     (rnrs io ports))

        (define (call-with-input-pipe command proc)
          "Call @var{proc} with input pipe to @var{command}. @var{command} is a list of
program arguments."
          (match command
            ((prog args ...)
             (let ((port #f))
               (dynamic-wind
                 (lambda ()
                   (set! port (apply open-pipe* OPEN_READ prog args)))
                 (cut proc port)
                 (cut close-pipe port))))))

        (define (git-version)
          (call-with-input-pipe (list "git" "tag" "--sort=-taggerdate" "--list" "v*")
            (compose (cut substring <> (string-length "v"))
                     get-line)))

        (set-path-environment-variable
         "PATH" '("bin") '(#$git-minimal #$gnu-make #$lzip))
        (invoke "git" "clone" (string-append "file://" #$ravanan-git-repo) (getcwd))
        (let ((version (git-version)))
          (invoke "make" "dist" (string-append "version=" version))
          (copy-file (string-append "ravanan-" version ".tar.lz")
                     #$output)))))

(define ravanan-release
  (computed-file "ravanan.tar.lz"
                 ravanan-release-gexp))

ravanan-release
