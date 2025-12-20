(command #:inputs (message #:type string)
         #:run "echo" ("-a" message #:separate? #f) ("-b" "bar" #:separate? #f)
         #:outputs (output_message #:type stdout))
