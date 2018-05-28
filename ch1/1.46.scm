(define (iterative-improvement good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter first-guess)))

(define (sqrt x)
  (let ((tolerance 0.000001))
    (define (average x y) (/ (+ x y) 2))
    ((iterative-improvement
      (lambda (guess) (< (abs (- (square guess) x)) tolerance))
      (lambda (guess) (average guess (/ x guess))))
     1.0)))

(define (fixed-point f first-guess)
  (let ((tolerance 0.000001))
    (define (improve x) (f x))
    (define (good-enough? x) (< (abs (- (improve x) x)) tolerance))
    ((iterative-improvement
      good-enough?
      improve)
     first-guess)))
