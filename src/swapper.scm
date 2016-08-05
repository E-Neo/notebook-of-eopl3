;;; swapper : Sym * Sym * S-list -> S-list
;;; usage: (swapper s1 s2 slist) returns a list the same as slist,
;;;        but with all occurrences of s1 replaced by s2 and all
;;;        occurrences of s2 replaced by s1.

(define (swapper s1 s2 slist)
  ;; swapper-in-s-exp : Sym * Sym * S-exp -> S-exp
  ;; usage: (swapper-in-s-exp s1 s2 sexp) returns a S-exp the same
  ;;        as sexp but with all occurrences of s1 replaced by s2
  ;;        and all occurrences of s2 replaced by s1.
  (define (swapper-in-s-exp s1 s2 sexp)
    (if (symbol? sexp)
        (cond
         ((eqv? s1 sexp) s2)
         ((eqv? s2 sexp) s1)
         (else sexp))
        (swapper s1 s2 sexp)))
  (if (null? slist)
      '()
      (cons (swapper-in-s-exp s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))
