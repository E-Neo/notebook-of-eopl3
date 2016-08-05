;;; vector-sum : Vectorof(Int) -> Int

(define (vector-sum v)
  ;; partial-vector-sum : Vectorof(Int) * Int -> Int
  ;; usage: if 0 <= n < length(v), then
  ;;        (partial-vector-sum v n) = \sum_{i=0}^{n}{v_i}
  (define (partial-vector-sum v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (1- n)))))
  (let ((n (vector-length v)))
    (if (zero? n)
        0
        (partial-vector-sum v (1- n)))))
