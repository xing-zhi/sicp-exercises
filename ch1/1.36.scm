(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? guess1 guess2)
    (< (abs (- guess1 guess2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compute-without-average-damping)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 10))
(compute-without-average-damping)
;;; 33步

(define (compute-with-average-damping)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 10))
(compute-with-average-damping)
;;; 9步

;;; 以10为初始猜测值，不使用average damping，需要进行33次猜测得到结果；使用average damping，只需要9次猜测就得到结果。由此可见，使用average damping可以很好地加快向真实值的逼近。
