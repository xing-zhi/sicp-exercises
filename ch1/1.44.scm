(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (define (iter result i)
    (if (= i 1)
        result
        (iter (compose f result) (- i 1))))
  (iter f n))

(define (smooth f)
  (let ((dx 0.0000001))
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))
