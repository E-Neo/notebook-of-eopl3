;;; lambda-calculus expression


(define (var-exp var)
  (list 'var-exp var))
(define (lambda-exp bound-var body)
  (list 'lambda-exp bound-var body))
(define (app-exp rator rand)
  (list 'app-exp rator rand))

(define (var-exp? exp)
  (eqv? (car exp) 'var-exp))
(define (lambda-exp? exp)
  (eqv? (car exp) 'lambda-exp))
(define (app-exp? exp)
  (eqv? (car exp) 'app-exp))

(define (var-exp->var exp)
  (car (cdr exp)))
(define (lambda-exp->bound-var exp)
  (car (cdr exp)))
(define (lambda-exp->body exp)
  (car (cdr (cdr exp))))
(define (app-exp->rator exp)
  (car (cdr exp)))
(define (app-exp->rand exp)
  (car (cdr (cdr exp))))


(define (occurs-free? search-var exp)
  (cond ((var-exp? exp)
         (eqv? search-var (var-exp->var exp)))
        ((lambda-exp? exp)
         (and (not (eqv? search-var
                         (lambda-exp->bound-var exp)))
              (occurs-free? search-var (lambda-exp->body exp))))
        (else (or (occurs-free? search-var (app-exp->rator exp))
                  (occurs-free? search-var (app-exp->rand exp))))))
