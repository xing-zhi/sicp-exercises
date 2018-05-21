(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? guess1 guess2)
    (< (abs (- guess1 guess2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (phi)
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1))
