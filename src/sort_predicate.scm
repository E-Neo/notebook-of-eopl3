;;; sort/predicate : Pred * Listof(Int) -> Listof(Int)
;;; usage: (sort/predicate pred loi) returns a list of elements sorted
;;;        by the predicate.

(define (sort/predicate pred loi)
  (define (merge pred loi1 loi2)
    (cond
     ((null? loi1) loi2)
     ((null? loi2) loi1)
     (else
      (if (pred (car loi1) (car loi2))
          (cons (car loi1) (merge pred (cdr loi1) loi2))
          (cons (car loi2) (merge pred loi1 (cdr loi2)))))))
  (if (null? loi)
      '()
      (merge pred (list (car loi)) (sort/predicate pred (cdr loi)))))
