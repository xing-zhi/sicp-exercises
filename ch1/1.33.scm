(define (filter-accumulate combiner null-value term a next b predicate?)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate? a)
                  (combiner (term a) result)
                  result))))
  (iter (term a) null-value))

(define (inc a) (+ a 1))
(define (square a) (* a a))
(define (identity a) a)

;;; prime? from page 66
(define (prime? n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n) (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (sum-of-squares-of-prime a b)
  (filter-accumulate + 0 square a inc b prime?))

;;; gcd from page 63
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-relatively-prime n)
  (define (relatively-prime? a)
    (= (gcd n a) 1))
  (filter-accumulate * 1 identity 1 inc n relatively-prime?))
