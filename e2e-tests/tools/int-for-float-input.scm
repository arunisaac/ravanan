(command #:inputs (message1 #:type float) (message2 #:type float)
         #:run "echo" message1 message2
         #:outputs (output_message #:type stdout))
