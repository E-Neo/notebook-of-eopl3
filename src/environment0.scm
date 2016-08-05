;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

;;; empty-env : () -> Env

(define (empty-env)
  (list 'empty-env))


;;; extend-env : Var * SchemeVal * Env -> Env

(define (extend-env var val env)
  (list 'extend-env var val env))


;;; apply-env : Env * Var -> SchemeVal

(define (apply-env env search-var)
  (define (report-no-binding-found search-var)
    (error 'apply-env "no binding-found for" search-var))
  (define (report-invalid-env env)
    (error 'apply-env "bad environment" env))
  (cond
   ((eqv? (car env) 'empty-env)
    (report-no-binding-found search-var))
   ((eqv? (car env) 'extend-env)
    (let ((saved-var (cadr env))
          (saved-val (caddr env))
          (saved-env (cadddr env)))
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var))))
   (else
    (report-invalid-env env))))
