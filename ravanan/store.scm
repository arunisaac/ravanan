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
  #:use-module (ice-9 filesystem)
  #:use-module (ravanan work command-line-tool)
  #:use-module (ravanan work monads)
  #:use-module (ravanan work vectors)
  #:export (%store-files-directory
            %store-data-directory
            %store-logs-directory

            make-store
            script->store-files-directory
            script->store-data-file
            script->store-stdout-file
            script->store-stderr-file
            intern-file
            store-item-name))

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
    (format (current-error-port)
            "store ~a does not exist; creating it~%"
            store)
    (make-directories store)
    (for-each (lambda (directory)
                (mkdir (expand-file-name directory store)))
              (list %store-files-directory
                    %store-data-directory
                    %store-logs-directory))))

(define (script->store-files-directory script store)
  "Return the store files directory in @var{store} corresponding to @var{script}
path."
  (expand-file-name (file-name-join* %store-files-directory
                                     (basename script))
                    store))

(define (script->store-data-file script store)
  "Return the store data file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-data-directory
                                     (string-append (basename script) ".json"))
                    store))

(define (script->store-stdout-file script store)
  "Return the store stdout file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append (basename script) ".stdout"))
                    store))

(define (script->store-stderr-file script store)
  "Return the store stderr file in @var{store} corresponding to @var{script} path."
  (expand-file-name (file-name-join* %store-logs-directory
                                     (string-append (basename script) ".stderr"))
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
                   (string-drop checksum (string-length "sha1$"))
                   (sha1-hash path)))
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
                                       (string-append sha1
                                                      "-"
                                                      (basename path)))
                      store)))
                (if (file-exists? interned-path)
                    (format (current-error-port)
                            "~a previously interned into store as ~a~%"
                            path interned-path)
                    (begin
                      (format (current-error-port)
                              "Interning ~a into store as ~a~%"
                              path interned-path)
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

;; Length of a base-16 encoded SHA1 hash
(define %store-hash-length 40)

(define (store-item-name path)
  "Return the basename of store item @var{path} with the store hash stripped out."
  (string-drop (basename path)
               ;; the hash and the dash after the hash
               (1+ %store-hash-length)))
