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

(define-module (ravanan utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 filesystem)
  #:use-module (ice-9 match)
  #:export (string-trim-prefix
            load-script))

(define (string-trim-prefix prefix str)
  "Remove @var{prefix} from @var{str} if it exists. Else, return @var{str} as is."
  (if (string-prefix? prefix str)
      (substring str (string-length prefix))
      str))

(define* (load-script script-file #:key (modules '()))
  "Load script from @var{script-file} and return its value. Import @var{modules}
before loading script."
  ;; We load the script file into a dummy module of its own so that any
  ;; definitions from there don't leak into this module. We also ensure that
  ;; this dummy module is different for different script files so that
  ;; definitions from one script file don't leak into other script files.
  (let ((script-module
         (resolve-module (match (file-name-split (canonicalize-file-name script-file))
                           (("" parts ...)
                            (map string->symbol parts))))))
    ;; Import modules required for loading script.
    (for-each (lambda (module-name)
                (module-use! script-module (resolve-interface module-name)))
              modules)
    (save-module-excursion
     (lambda ()
       (set-current-module script-module)
       ;; Do not auto-compile script file.
       (set! %load-should-auto-compile #f)
       ;; Our use of load triggers a "Add #:declarative? #f to your
       ;; define-module invocation" warning during compilation. But, it is
       ;; probably safe to ignore this warning since we use load only within a
       ;; dummy module.
       (load (canonicalize-path script-file))))))
