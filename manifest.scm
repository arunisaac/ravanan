(use-modules ((gnu packages bioinformatics) #:select (ccwl))
             ((gnu packages guile-xyz) #:select (guile-ares-rs))
             ((gnu packages task-management) #:select (git-bug))
             ((cwltest-package) #:select (cwltest))
             ((ravanan-package) #:select (ravanan))
             (srfi srfi-1))

(define (manifest-cons* . args)
  "ARGS is of the form (PACKAGES ... ONTO-MANIFEST). Return a manifest
with PACKAGES and all packages in ONTO-MANIFEST."
  (let ((packages (drop-right args 1))
        (onto-manifest (last args)))
    (manifest (append (map package->manifest-entry packages)
                      (manifest-entries onto-manifest)))))

(manifest-cons* ccwl
                cwltest
                git-bug
                guile-ares-rs
                (package->development-manifest ravanan))
