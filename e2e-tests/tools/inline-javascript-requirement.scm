(command #:inputs (number #:type int)
         #:run "echo" "$(1 + inputs.number)"
         #:outputs (sum #:type stdout)
         #:requirements ((InlineJavascriptRequirement)))
