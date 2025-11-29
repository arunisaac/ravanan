(command #:inputs (message #:type string) (no-newline? #:type boolean)
         #:run "echo" ("-n" no-newline?) message
         #:outputs (output_message #:type stdout))
