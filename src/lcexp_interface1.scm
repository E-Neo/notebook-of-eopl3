;;; lambda-calculus expression

(use-modules (srfi srfi-9))


(define-record-type var-exp
  (make-var-exp var)
  var-exp?
  (var var-exp->var))

(define-record-type lambda-exp
  (make-lambda-exp bound-var body)
  lambda-exp?
  (bound-var lambda-exp->bound-var)
  (body lambda-exp->body))

(define-record-type app-exp
  (make-app-exp rator rand)
  app-exp?
  (rator app-exp->rator)
  (rand app-exp->rand))


(define (occurs-free? search-var exp)
  (cond
   ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
   ((lambda-exp? exp)
    (and (not (eqv? search-var (lambda-exp->bound-var exp)))
         (occurs-free? search-var (lambda-exp->body exp))))
   (else
    (or (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp))))))
