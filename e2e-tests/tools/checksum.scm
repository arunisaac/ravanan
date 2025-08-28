(define md5sum
  (command #:inputs (file #:type File #:stage? #t)
           #:run "md5sum" "$(inputs.file.basename)"
           #:outputs (md5 #:type stdout)
           #:stdout "md5"))

(define sha1sum
  (command #:inputs (file #:type File #:stage? #t)
           #:run "sha1sum" "$(inputs.file.basename)"
           #:outputs (sha1 #:type stdout)
           #:stdout "sha1"))

(define sha256sum
  (command #:inputs (file #:type File #:stage? #t)
           #:run "sha256sum" "$(inputs.file.basename)"
           #:outputs (sha256 #:type stdout)
           #:stdout "sha256"))

(workflow ((file #:type File))
  (tee (md5sum #:file file)
       (sha1sum #:file file)
       (sha256sum #:file file)))
