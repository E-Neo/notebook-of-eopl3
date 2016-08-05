;;; lambda-calculus expression


;;; the constructors:
;;; =================

;;; var-exp : Var -> Lc-exp

(define (var-exp var) var)

;;; lambda-exp : Var * Lc-exp -> Lc-exp

(define (lambda-exp var exp)
  `(lambda (,var) ,exp))

;;; app-exp : Lc-exp * Lc-exp -> Lc-exp

(define (app-exp exp1 exp2)
  `(,exp1 ,exp2))


;;; the predicates:
;;; ===============

;;; var-exp? : Lc-exp -> Bool

(define (var-exp? exp)
  (symbol? exp))

;;; lambda-exp? : Lc-exp -> Bool

(define (lambda-exp? exp)
  (eqv? 'lambda (car exp)))

;;; app-exp? : Lc-exp -> Bool

(define (app-exp? exp)
  (if (or (var-exp? exp)
          (lambda-exp? exp))
      #f
      #t))


;;; the extractors:
;;; ===============

;;; var-exp->var : Lc-exp -> Var

(define (var-exp->var exp) exp)

;;; lambda-exp->bound-var : Lc-exp -> Var

(define (lambda-exp->bound-var exp) (caadr exp))

;;; lambda-exp->body : Lc-exp -> Lc-exp

(define (lambda-exp->body exp) (caddr exp))

;;; app-exp->rator : Lc-exp -> Lc-exp

(define (app-exp->rator exp) (car exp))

;;; app-exp->rand : Lc-exp -> Lc-exp

(define (app-exp->rand exp) (cadr exp))


;;; ===============


;;; occurs-free? : Sym * LcExp -> Bool

(define (occurs-free? search-var exp)
  (cond ((var-exp? exp)
         (eqv? search-var (var-exp->var exp)))
        ((lambda-exp? exp)
         (and (not (eqv? search-var
                         (lambda-exp->bound-var exp)))
              (occurs-free? search-var (lambda-exp->body exp))))
        (else (or (occurs-free? search-var (app-exp->rator exp))
                  (occurs-free? search-var (app-exp->rand exp))))))
