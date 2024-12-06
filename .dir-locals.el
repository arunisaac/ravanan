;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode))
 (makefile-gmake-mode
  (indent-tabs-mode t))
 (scheme-mode
  (fill-column . 80)
  (eval . (put 'assoc-set 'scheme-indent-function 1))
  (eval . (put 'build-gexp-script 'scheme-indent-function 1))
  (eval . (put 'call-with-current-directory 'scheme-indent-function 1))
  (eval . (put 'call-with-inferior 'scheme-indent-function 1))
  (eval . (put 'maybe-let* 'scheme-indent-function 1))
  (eval . (put 'maybe-assoc-set 'scheme-indent-function 1))))
