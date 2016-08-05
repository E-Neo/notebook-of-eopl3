;;; sort : Listof(Int) -> Listof(Int)
;;; usage: (sort loi) returns a list of the elements of
;;;        loi in ascending order.

(define (sort loi)
  (define (merge loi1 loi2)
    (cond
     ((null? loi1) loi2)
     ((null? loi2) loi1)
     (else
      (if (< (car loi1) (car loi2))
          (cons (car loi1) (merge (cdr loi1) loi2))
          (cons (car loi2) (merge loi1 (cdr loi2)))))))
  (if (null? loi)
      '()
      (merge (list (car loi)) (sort (cdr loi)))))
