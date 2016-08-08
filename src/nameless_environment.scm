;;; Nameless environment


(define-module (eopl nameless-environment)
  #:use-module (eopl datatype)
  #:export (;; environment
            nameless-environment?
            empty-nameless-env
            extend-nameless-env
            apply-nameless-env
            ;; program
            program
            program?
            a-program
            ;; expression
            expression
            expression?
            const-exp
            diff-exp
            zero?-exp
            if-exp
            var-exp
            let-exp
            proc-exp
            call-exp
            nameless-var-exp
            nameless-let-exp
            nameless-proc-exp
            ;; proc
            proc
            proc?
            eopl-procedure
            ;; expval
            expval
            expval?
            num-val
            bool-val
            proc-val))


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
  (nameless-var-exp
   (n integer?))
  (nameless-let-exp
   (exp1 expression?)
   (body expression?))
  (nameless-proc-exp
   (body expression?)))

(define-datatype proc proc?
  (eopl-procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))


(define (list-of pred)
  (lambda (val)
    (or (null? val)
        (and (pair? val)
             (pred (car val))
             ((list-of pred) (cdr val))))))


;;; nameless-environment? : SchemeVal -> Bool

(define (nameless-environment? x)
  ((list-of expval?) x))


;;; empty-nameless-env : () -> Nameless-env


(define (empty-nameless-env) '())


;;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env

(define (extend-nameless-env val nameless-env)
  (cons val nameless-env))


;;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal

(define (apply-nameless-env nameless-env n)
  (list-ref nameless-env n))
