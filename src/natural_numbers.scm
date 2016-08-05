;;; Unary representation

(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define successer (lambda (n) (cons #t n)))
(define predecessor (lambda (n) (cdr n)))


;;; Scheme number representation

(define zero (lambda () 0))
(define is-zero? (lambda (n) (zero? n)))
(define successer (lambda (n) (1+ n)))
(define predecessor (lambda (n) (1- n)))


;;; Bignum representation

(define base 2)
(define zero '())
(define is-zero? (lambda (n) (null? n)))
(define successer
  (lambda (n)
    (if (is-zero? n)
        (list 1)
        (let ((r (car n))
              (q (cdr n)))
          (if (= r (1- base))
              (cons 0 (successer q))
              (cons (1+ r) q))))))
(define predecessor
  (lambda (n)
    (let ((r (car n))
          (q (cdr n)))
      (if (zero? r)
          (cons (1- base) (predecessor q))
          (cons (1- r) q)))))
