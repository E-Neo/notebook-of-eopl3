;;; list-set : Listof(SchemeVal) * Int * SchemeVal
;;;            Listof(SchemeVal)
;;; usage: (list-set lst n x) returns a list like lst,
;;;        except that the n-th element, using zero-based
;;;        indexing, is x.

(define (list-set lst n x)
  ;; list-first-n : Listof(SchemeVal) * Int -> Listof(SchemeVal)
  ;; usage: (list-first-n lst n) returns the first n elements of lst.
  (define (list-first-n lst n)
    (if (zero? n)
        '()
        (cons (car lst)
              (list-first-n (cdr lst) (1- n)))))
  ;; list-from-n : Listof(SchemeVal) * Int -> Listof(SchemeVal)
  ;; usage: (list-from-n '(x0 x1 ... xn ...) n) = (xn ...)
  (define (list-from-n lst n)
    (if (zero? n)
        lst
        (cdr (list-from-n lst (1- n)))))
  (append (list-first-n lst n)
          (cons x (list-from-n lst (1+ n)))))
