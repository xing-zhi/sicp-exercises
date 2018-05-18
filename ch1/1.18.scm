(define (double n) (+ n n))
(define (halve n) (/ n 2))
(define (* a b)
  (define (iter a b product)
    (cond ((= b 0) product)
          ((even? b) (iter (double a) (halve b) product))
          (else (iter a (- b 1) (+ product a)))))
  (if (> b 0)
      (iter a b 0)
      (iter (- a) (- b) 0)))