(command #:inputs (message #:type string
                           #:default "Hello world!")
         #:run "echo" message
         #:outputs (output_message #:type stdout))
