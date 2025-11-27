(define echo
  (command #:inputs (message1 #:type string) (message2 #:type string)
           #:run "echo" message1 message2
           #:outputs (output_message #:type stdout)))

(define echo-workflow
  (workflow ((message1 #:type string)
             (message2 #:type string
                       #:default "Hello world!"))
    (echo #:message1 message1
          #:message2 message2)))

(workflow ((message #:type string))
  (echo-workflow #:message1 message))
