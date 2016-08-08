;;; The Translator : takes a program and removes all the
;;;                  variables from the declarations, and
;;;                  replaces every variable reference by
;;;                  its lexical depth.


(define-module (eopl translator)
  #:use-module (eopl datatype)
  #:use-module (eopl static-environment)
  #:use-module (eopl nameless-environment)
  #:export (translation-of-program))


;;; init-senv : () -> Senv

(define (init-senv)
  (empty-senv))


;;; translation-of-program : Program -> Nameless-program

(define (translation-of-program pgm)
  (cases program pgm
         (a-program (exp1)
                    (a-program
                     (translation-of exp1 (init-senv))))))


;;; translation-of : Exp * Senv -> Nameless-exp

(define (translation-of exp senv)
  (define (report-invalid-source-expression exp)
    (error "invalid source expression" exp))
  (cases expression exp
         (const-exp (num) (const-exp num))
         (diff-exp (exp1 exp2)
                   (diff-exp (translation-of exp1 senv)
                             (translation-of exp2 senv)))
         (zero?-exp (exp1)
                    (zero?-exp (translation-of exp1 senv)))
         (if-exp (exp1 exp2 exp3)
                 (if-exp (translation-of exp1 senv)
                         (translation-of exp2 senv)
                         (translation-of exp3 senv)))
         (var-exp (var)
                  (nameless-var-exp (apply-senv senv var)))
         (let-exp (var exp1 body)
                  (nameless-let-exp
                   (translation-of exp1 senv)
                   (translation-of body
                                   (extend-senv var senv))))
         (proc-exp (var body)
                   (nameless-proc-exp
                    (translation-of body
                                    (extend-senv var senv))))
         (call-exp (rator rand)
                   (call-exp (translation-of rator senv)
                             (translation-of rand senv)))
         (else (report-invalid-source-expression exp))))
