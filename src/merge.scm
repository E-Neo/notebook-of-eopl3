;;; merge : Listof(Int) * Listof(Int) -> Listof(Int)
;;; usage: (merge loi1 loi2) where loi1 and loi2 are lists of integers
;;;        that are sorted in ascending order, returns a sorted list
;;;        of all the integers in loi1 and loi2.

(define (merge loi1 loi2)
  (cond
   ((null? loi1) loi2)
   ((null? loi2) loi1)
   (else
    (if (< (car loi1) (car loi2))
        (cons (car loi1) (merge (cdr loi1) loi2))
        (cons (car loi2) (merge loi1 (cdr loi2)))))))
