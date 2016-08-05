;;; list-sum : Listof(Int) -> Int

(define (list-sum loi)
  (if (null? loi)
      0
      (+ (car loi)
         (list-sum (cdr loi)))))
