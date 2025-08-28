(define print
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (printed_message #:type stdout)
           #:stdout "printed-message-output.txt"))

(workflow ((message #:type string))
  (print #:message message))
