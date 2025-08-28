(command #:inputs (last #:type int) (separator #:type string)
         #:run "seq" ("-s" separator) last
         #:outputs (sequence #:type stdout))
