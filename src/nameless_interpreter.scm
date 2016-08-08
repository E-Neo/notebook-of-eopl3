;;; The Nameless Interpreter


(use-modules (eopl datatype)
             (eopl nameless-environment)
             (eopl translator))


(define (report-expval-extractor-error field-name val)
  (error "ExpVal extractro error" field-name val))
(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (report-expval-extractor-error 'num val))))
(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (report-expval-extractor-error 'bool val))))
(define (expval->proc val)
  (cases expval val
         (proc-val (proc) proc)
         (else (report-expval-extractor-error 'bool val))))


(define (apply-procedure proc1 val)
  (cases proc proc1
         (eopl-procedure
          (body saved-nameless-env)
          (value-of
           body (extend-nameless-env
                 val saved-nameless-env)))))


(define (init-env) (empty-nameless-env))


;;; value-of-program : Program -> ExpVal

(define (value-of-program pgm)
  (cases program (translation-of-program pgm)
         (a-program (exp1)
                    (value-of exp1 (init-env)))))


(define (value-of exp nameless-env)
  (define (report-invalid-translated-expression exp)
    (error "invalid translated expression" exp))
  (cases expression exp
         (const-exp (num) (num-val num))
         (diff-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 nameless-env))
                         (val2 (value-of exp2 nameless-env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (num-val (- num1 num2)))))
         (zero?-exp (exp1)
                    (let ((val1 (value-of exp1 nameless-env)))
                      (let ((num1 (expval->num val1)))
                        (if (zero? num1)
                            (bool-val #t)
                            (bool-val #f)))))
         (if-exp (exp1 exp2 exp3)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (if (expval->bool val1)
                       (value-of exp2 nameless-env)
                       (value-of exp3 nameless-env))))
         (call-exp (rator rand)
                   (let ((proc (expval->proc (value-of rator nameless-env)))
                         (arg (value-of rand nameless-env)))
                     (apply-procedure proc arg)))
         (nameless-var-exp (n)
                           (apply-nameless-env nameless-env n))
         (nameless-let-exp (exp1 body)
                           (let ((val (value-of exp1 nameless-env)))
                             (value-of body
                                       (extend-nameless-env val nameless-env))))
         (nameless-proc-exp (body)
                            (proc-val (eopl-procedure body nameless-env)))
         (else (report-invalid-translated-expression exp))))
