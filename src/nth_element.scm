;;; nth-element : List * Int -> SchemeVal
;;; usage: (nth-element lst n) = the t-th element of lst

(define (nth-element lst n)
  (define (report-list-too-short n)
    (error "Argument out of range:" n))
  (if (null? lst)
      (report-list-too-short n)
      (if (zero? n)
          (car lst)
          (nth-element (cdr lst) (1- n)))))
