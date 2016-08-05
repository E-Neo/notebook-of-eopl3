;;; down : Listof(SchemeVal) -> Listof(SchemeVal)
;;; usage: (down '(x0 x1 x2 ...)) = ((x0) (x1) (x2) ...)

(define (down lst)
  (if (null? lst)
      '()
      (cons (list (car lst))
            (down (cdr lst)))))
