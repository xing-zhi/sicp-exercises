;;; accumulate generates a iterative process
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter (term a) null-value))

;;; define sum using accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

;;; define product using accumulate
(define (product term a next b)
  (accumulate * 1 term a next b))

;;; accumulate generates a recursive process
(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-recursive combiner null-value term (next a) next b))))
