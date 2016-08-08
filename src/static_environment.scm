;;; Static environments
;;; Senv = Listof(Sym)
;;; Lexaddr = N


(define-module (eopl static-environment)
  #:export (empty-senv
            extend-senv
            apply-senv))


;;; empty-senv : () -> Senv

(define (empty-senv) '())


;;; extend-senv : Var * Senv -> Senv

(define (extend-senv var senv)
  (cons var senv))


;;; apply-senv : Senv * Var -> Lexaddr

(define (apply-senv senv var)
  (define (report-unbound-var var)
    (error "unbound var" var))
  (cond ((null? senv)
         (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
         (1+ (apply-senv (cdr senv) var)))))
