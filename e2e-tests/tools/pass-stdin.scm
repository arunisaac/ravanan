(define count-bytes
  (command #:inputs (file #:type File)
           #:run "wc" "-c"
           #:outputs (bytes #:type stdout)
           #:stdin file))

(workflow ((file #:type File))
  (count-bytes #:file file))
