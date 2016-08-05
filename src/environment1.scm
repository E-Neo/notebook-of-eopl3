;;; Env = Var -> SchemeVal

;;; empty-env : () -> Env

(define (empty-env)
  (define (report-no-binding-found search-var)
    (error 'apply-env "no binding-found for" search-var))
  (lambda (search-var)
    (report-no-binding-found search-var)))


;;; extend-env : Var * SchemeVal * Env -> Env

(define (extend-env saved-var saved-val saved-env)
  (lambda (search-var)
    (if (eqv? search-var saved-var)
        saved-val
        (apply-env saved-env search-var))))


;;; apply-env : Env * Val -> SchemeVal

(define (apply-env env search-var)
  (env search-var))
