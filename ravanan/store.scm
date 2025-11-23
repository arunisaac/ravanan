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

(define-module (ravanan store)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 filesystem)
  #:use-module (gcrypt hash)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (guix build utils)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work ui)
  #:use-module (ravanan work utils)
  #:use-module (ravanan work vectors)
  #:export (%store-files-directory
            %store-data-directory
            %store-logs-directory

            make-store
            step-store-files-directory
            step-store-data-file
            step-store-stdout-file
            step-store-stderr-file
            intern-file))

(define %store-files-directory
  "files")

(define %store-data-directory
  "data")

(define %store-logs-directory
  "logs")

(define (make-store store)
  "Make @var{store} directory and initialize with subdirectories. If @var{store}
already exists, do nothing."
  (unless (file-exists? store)
    (log-warning "store ~a does not exist; creating it"
                 store)
    (make-directories store)
    (for-each (lambda (directory)
                (mkdir (expand-file-name directory store)))
              (list %store-files-directory
                    %store-data-directory
                    %store-logs-directory))))

(define (sha1-hash-sexp tree)
  (bytevector->base32-string
   (let ((port get-hash (open-hash-port (hash-algorithm sha1))))
     ;; tree should probably be canonicalized using canonical S-expressions or
     ;; similar. But, it doesn't matter much for our purposes. write already
     ;; canonicalizes in a way. In the unlikely case of a problem, the worst
     ;; that can happen is that we recompute all steps of the workflow.
     (write tree port)
     (close port)
     (get-hash))))

(define (step-store-basename script inputs)
  "Return the basename in the store for files of CWL step with @var{script} and
@var{inputs}."
  (string-append (sha1-hash-sexp (cons script (canonicalize-json inputs)))
                 "-"
                 (strip-store-file-name script)))

(define (step-store-files-directory script inputs store)
  "Return the @var{store} files directory for CWL step with @var{script} and
@var{inputs}."
  (expand-file-name (file-name-join* %store-files-directory
                                     (step-store-basename script inputs))
                    store))

(define (step-store-data-file script inputs store)
  "Return the @var{store} data file for CWL step with @var{script} and
@var{inputs}."
  (expand-file-name (file-name-join* %store-data-directory
                                     (string-append
                                      (step-store-basename script inputs)
                                      ".json"))
                    store))

(define (step-store-stdout-file script inputs store)
  "Return the @var{store} stdout file for CWL step with @var{script} and
@var{inputs}."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append
                                      (step-store-basename script inputs)
                                      ".stdout"))
                    store))

(define (step-store-stderr-file script inputs store)
  "Return the @var{store} stderr file for CWL step with @var{script} and
@var{inputs}."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append
                                      (step-store-basename script inputs)
                                      ".stderr"))
                    store))

(define (same-filesystem? path1 path2)
  "Return @code{#t} if @var{path1} and @var{path2} are on the same filesystem.
Else, return @code{#f}."
  (= (stat:dev (stat path1))
     (stat:dev (stat path2))))

(define (intern-file file store)
  "Intern @code{File} type object @var{file} into the ravanan @var{store} unless it
is already a store path. Return an updated @code{File} type object with the
interned path and location."
  (let* ((path (assoc-ref file "path"))
         (checksum (assoc-ref file "checksum"))
         (sha1 (if (and checksum
                        (string-prefix? "sha1$" checksum))
                   (base16-string->bytevector
                    (string-drop checksum (string-length "sha1$")))
                   (sha1-hash-bytes path)))
         (interned-path
          (if (string-prefix? store path)
              ;; If file is already a store path, return it as is.
              path
              ;; Else, intern it if it isn't already and return the interned
              ;; path. Not re-interning already interned files saves us a lot of
              ;; time especially with large files.
              (let ((interned-path
                     (expand-file-name
                      (file-name-join* %store-files-directory
                                       (string-append (bytevector->base32-string sha1)
                                                      "-"
                                                      (basename path))
                                       (basename path))
                      store)))
                (if (file-exists? interned-path)
                    (log-debug "~a previously interned into store as ~a~%"
                               path interned-path)
                    (begin
                      (log-info "Interning ~a into store as ~a~%"
                                path interned-path)
                      (mkdir (dirname interned-path))
                      ;; Hard link if on the same filesystem. Else, copy.
                      ((if (same-filesystem? path
                                             (expand-file-name %store-files-directory
                                                               store))
                           link
                           copy-file)
                       path interned-path)))
                interned-path))))
    (maybe-assoc-set file
      (cons "location" (just (string-append "file://" interned-path)))
      (cons "path" (just interned-path))
      (cons "basename" (just (basename interned-path)))
      (cons "nameroot" (just (file-name-stem interned-path)))
      (cons "nameext" (just (file-name-extension interned-path)))
      (cons "secondaryFiles"
            (maybe-let* ((secondary-files (maybe-assoc-ref (just file)
                                                           "secondaryFiles")))
              (just (vector-map (cut intern-file <> store)
                                secondary-files)))))))


