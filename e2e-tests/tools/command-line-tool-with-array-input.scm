(command #:inputs (messages #:type (array string))
         #:run "echo" messages
         #:outputs (output_message #:type stdout))
