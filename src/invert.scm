;;; invert : Listof(2-lists) -> Listof(2-lists)
;;;          2-lists: lists of length two
;;; usage: (invert '((a0 b0) (a1 b1) ...)) = ((b0 a0) (b1 a1) ...)

(define (invert lst)
  (if (null? lst)
      '()
      (let ((first (car lst))
            (rest (cdr lst)))
        (cons (list (cadr first) (car first))
              (invert rest)))))
