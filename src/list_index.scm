;;; list-index : Pred * List -> List
;;; usage: (list-index pred lst) returns the 0-based position
;;;        of the first element of lst that satisfies the predicate
;;;        pred. If no element of lst satisfies the predicate, then
;;;        list-index returns #f.

(define (list-index pred lst)
  ;; list-index-from : Int * Pred * List -> Int or #f
  ;; usage: (list-index-from n pred lst) returns the n-based position
  ;;        of the first element of lst that satisfies the predicate
  ;;        pred. If no element of lst satisfies the predicate, then
  ;;        list-index returns #f.

  (define (list-index-from n pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-from (1+ n) pred (cdr lst)))))
  (list-index-from 0 pred lst))
