(define example-program
  (a-program
   (letrec-exp 'double 'x
               (if-exp (zero?-exp (var-exp 'x))
                       (const-exp 0)
                       (diff-exp (call-exp (var-exp 'double)
                                           (diff-exp (var-exp 'x)
                                                     (const-exp 1)))
                                 (const-exp -2)))
               (call-exp (var-exp 'double) (const-exp 6)))))
