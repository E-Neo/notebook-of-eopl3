;;; S-list

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))

(define-datatype s-list s-list?
  (an-s-list
   (sexps (list-of s-exp?))))

(define (list-of pred)
  (lambda (val)
    (or (null? val)
        (and (pair? val)
             (pred (car val))
             ((list-of pred) (cdr val))))))
