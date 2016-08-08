;;; LETREC: A Language with Recursive Procedures


(use-modules (eopl datatype)
             (eopl environment))


(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (saved-env environment?)))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (proc-exp
   (var symbol?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-name symbol?)
   (b-var symbol?)
   (p-body expression?)
   (letrec-body expression?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))


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


;;; apply-procedure : Proc * ExpVal -> ExpVal

(define (apply-procedure proc1 val)
  (cases proc proc1
         (procedure (var body saved-env)
                    (value-of body (extend-env var val saved-env)))))


;;; init-env : () -> Env

(define (init-env)
  (empty-env))


;;; value-of-program : Program -> ExpVal

(define (value-of-program pgm)
  (cases program pgm
         (a-program (exp1)
                    (value-of exp1 (init-env)))))


;;; value-of : Exp * Env -> ExpVal

(define (value-of exp env)
  (cases expression exp
         (const-exp (num) (num-val num))
         (var-exp (var) (apply-env env var))
         (diff-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                         (val2 (value-of exp2 env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (num-val (- num1 num2)))))
         (zero?-exp (exp1)
                    (let ((val1 (value-of exp1 env)))
                      (let ((num1 (expval->num val1)))
                        (if (zero? num1)
                            (bool-val #t)
                            (bool-val #f)))))
         (if-exp (exp1 exp2 exp3)
                 (let ((val1 (value-of exp1 env)))
                   (if (expval->bool val1)
                       (value-of exp2 env)
                       (value-of exp3 env))))
         (let-exp (var exp1 body)
                  (let ((val1 (value-of exp1 env)))
                    (value-of body
                              (extend-env var val1 env))))
         (proc-exp (var body)
                   (proc-val (procedure var body env)))
         (call-exp (rator rand)
                   (let ((proc (expval->proc (value-of rator env)))
                         (arg (value-of rand env)))
                     (apply-procedure proc arg)))
         (letrec-exp (p-name b-var p-body letrec-body)
                     (value-of letrec-body
                               (extend-env-rec p-name b-var p-body
                                               env)))))
