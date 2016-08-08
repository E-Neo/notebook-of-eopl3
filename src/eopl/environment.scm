;;; environment


(define-module (eopl environment)
  #:use-module (eopl datatype)
  #:export (environment?
            empty-env
            extend-env
            extend-env-rec
            apply-env))


(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-var symbol?)
   (p-body expression?)
   (env environment?)))

(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (saved-env environment?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))


;;; apply-env : Env * Var -> SchemeVal

(define (apply-env env search-var)
  (define (report-no-binding-found search-var)
    (error 'apply-env "no binding-found for" search-var))
  (define (report-invalid-env env)
    (error 'apply-env "bad environment" env))
  (cases environment env
         (empty-env ()
                    (report-no-binding-found search-var))
         (extend-env (saved-var saved-val saved-env)
                     (if (eqv? saved-var search-var)
                         saved-val
                         (apply-env saved-env search-var)))
         (extend-env-rec (p-name b-var p-body saved-env)
                         (if (eqv? p-name search-var)
                             (proc-val (procedure b-var p-body env))
                             (apply-env saved-env search-var)))
         (else (report-invalid-env env))))
