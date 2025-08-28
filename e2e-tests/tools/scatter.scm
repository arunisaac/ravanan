(define print
  (command #:inputs (message #:type string) (other-message #:type string)
           #:run "echo" message other-message
           #:outputs (printed_output #:type stdout)))

(workflow ((message #:type string) (other_messages #:type (array string)))
  (scatter (print #:message message)
           #:other-message other_messages))
