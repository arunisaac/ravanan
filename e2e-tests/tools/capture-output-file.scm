(define extract
  (command #:inputs (archive #:type File)
           #:run "tar" "--no-same-owner" "--extract" "--file" archive
           #:outputs (extracted_file
                      #:type File
                      #:binding ((glob . "hello.txt")))))

(workflow ((archive #:type File))
  (extract #:archive archive))
