(define echo
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (output_message #:type stdout)))

(define echo-workflow
  (workflow ((message #:type string
                      #:default "Hello world!"))
    (echo #:message message)))

(workflow ()
  (echo-workflow))
