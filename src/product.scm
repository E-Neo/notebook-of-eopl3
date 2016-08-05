;;; product : Listof(Sym) * Listof(Sym) -> Listof(2-lists)
;;; usage: (product los1 los2) returns a list of 2-lists that
;;;        represents the Cartesian product of los1 and los2.

(define (product los1 los2)
  ;; product-of-sym-and-los : Sym * Listof(Sym) -> Listof(2-lists)
  ;; usage: (product-of-sym-and-los s '(s0 s1 ...))
  ;;        = ((s s0) (s s1) ...)
  (define (product-of-sym-and-los s los)
    (if (null? los)
        '()
        (cons (list s (car los))
              (product-of-sym-and-los s (cdr los)))))
  (if (null? los1)
      '()
      (append (product-of-sym-and-los (car los1) los2)
              (product (cdr los1) los2))))
