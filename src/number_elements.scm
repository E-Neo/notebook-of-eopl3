;;; number-elements : List -> Listof(List(Int, SchemeVal))
;;; usage: (number-elements '(v0 v1 v2 ...))
;;;        = ((0 v0) (1 v1) (2 v2) ...)

(define (number-elements lst)
  ;; number-elements : Listof(SchemeVal) * Int
  ;;                   -> Listof(List(Int, SchemeVal))
  ;; usage: (number-elements-from '(v0 v1 v2 ...) n)
  ;;        = ((n v0) (n+1 v1) (n+2 v2) ...)
  (define (number-elements-from lst n)
    (if (null? lst)
        '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (1+ n)))))
  (number-elements-from lst 0))
