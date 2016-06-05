(define (cube-root-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (cube-root-iter (improve guess x) x)))
(define (good-enough? guess improved)
  (< (abs (- guess improved)) 0.001))
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (average x y)
  (/ (+ x y) 2))
(define (cube-root x)
  (cube-root-iter 1.0 x))