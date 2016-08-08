(define example-program
  (a-program
   (let-exp 'x (const-exp 200)
    (let-exp 'f (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)))
     (let-exp 'x (const-exp 100)
      (let-exp 'g (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)))
        (diff-exp (call-exp (var-exp 'f) (const-exp 1))
                  (call-exp (var-exp 'g) (const-exp 1)))))))))
