;;; duple : Int * SchemeVal -> Listof(SchemeVal)
;;; usage: (duple n x) returns a list containing n copies of x.

(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (1- n) x))))
