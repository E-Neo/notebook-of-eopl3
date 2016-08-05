;;; count-occurrences : Sym * S-list -> Int
;;; usage: (count-occurrences s slist) returns the number of
;;;        occurrences of s in slist.

(define (count-occurrences s slist)
  ;; count-occurrences-in-s-exp :
  ;; usage: (count-occurrences-in-s-exp s sexp) returns the
  ;;        number of occurrences of s in sexp.
  (define (count-occurrences-in-s-exp s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp) 1 0)
        (count-occurrences s sexp)))
  (if (null? slist)
      0
      (+ (count-occurrences-in-s-exp s (car slist))
         (count-occurrences s (cdr slist)))))
