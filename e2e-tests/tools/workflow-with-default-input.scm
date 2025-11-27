(define echo
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (output_message #:type stdout)))

(workflow ((message #:type string
                    #:default "Hello world!"))
  (echo #:message message))
