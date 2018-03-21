(define (fast-expt b n)
  (define (iter a current-base n)
    (cond ((= n 0) a)
          ((even? n)
           ;; b^(2n) = (b^2)^n
           (iter a (square current-base) (/ n 2)))
          (else
           ;; b^(n+1) = b*b^n
           (iter (* a current-base) current-base (- n 1)))))
  (iter 1 b n))
