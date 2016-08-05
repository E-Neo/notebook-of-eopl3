;;; LET Language

(load "environment0.scm")
(load "datatype.scm")

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
   (body expression?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))


;;; init-env : () -> Env
;;; usage: (init-env) = [i=|1|, v=|5|, x=|10|]

(define (init-env)
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))


(define (report-expval-extractor-error field-name val)
  (error "ExpVal extractro error" field-name val))


;;; expval->num : ExpVal -> Int

(define (expval->num val)
  (cases expval val
         (num-val (num) num)
         (else (report-expval-extractor-error 'num val))))


;;; expval->bool : ExpVal -> Bool

(define (expval->bool val)
  (cases expval val
         (bool-val (bool) bool)
         (else (report-expval-extractor-error 'bool val))))


;;; scan&parse : String -> Program

(define (scan&parse string)
  ;; TODO
  (a-program
   (let-exp 'i (var-exp 'x)
            (if-exp (zero?-exp (const-exp 0))
                    (diff-exp (var-exp 'i) (var-exp 'x))
                    (var-exp 'i)))))


;;; run : String -> ExpVal

(define (run string)
  (value-of-program (scan&parse string)))


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
                              (extend-env var val1 env))))))
