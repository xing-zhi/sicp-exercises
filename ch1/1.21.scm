(define (smallest-divisor n)
  (define (divides? a b) (= (remainder a b) 0))
  (define (try test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (try (+ test-divisor 1)))))
  (try 2))

(smallest-divisor 199)
;;; => 199

(smallest-divisor 1999)
;;; => 1999

(smallest-divisor 19999)
;;; => 7
