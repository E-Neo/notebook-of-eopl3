;;; remove-first : Sym * Listof(Sym) -> Listof(Sym)
;;; usage: (remove-first s los) returns a list with the same
;;;        elements arranged in the same order as los, except
;;;        that the first occurrence of the symbol s is removed.

(define (remove-first s los)
  (if (null? los)
      '()
      (if (eqv? s (car los))
          (cdr los)
          (cons (car los) (remove-first s (cdr los))))))
