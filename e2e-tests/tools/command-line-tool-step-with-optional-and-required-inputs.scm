(define echo
  (command #:inputs (message1 #:type string) (message2 #:type string
                                                       #:default "Hello world!")
           #:run "echo" message1 message2
           #:outputs (output_message #:type stdout)))

(workflow ((message #:type string))
  (echo #:message1 message))
