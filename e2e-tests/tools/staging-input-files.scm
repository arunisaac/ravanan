(command #:inputs (file #:type File
                        #:stage? #t)
         #:run "cat" "./$(inputs.file.basename)"
         #:outputs (output_file #:type stdout))
