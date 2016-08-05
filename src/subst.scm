;;; subst : Sym * Sym * S-list -> S-list
;;; usage: (subst new old slist) returns a new list with all
;;;        occurrences of old replaced by instances of new.

(define (subst new old slist)
  ;; subst-in-sexp : Sym * Sym * S-exp -> S-exp
  (define (subst-in-sexp new old sexp)
    (if (symbol? sexp)
        (if (eqv? old sexp) new sexp)
        (subst new old sexp)))
  (if (null? slist)
      '()
      (cons (subst-in-sexp new old (car slist))
            (subst new old (cdr slist)))))
