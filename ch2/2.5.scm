(define (get-exponent-of-base base n)
  (define (iter n result)
    (if (= 0 (remainder n base))
        (iter (/ n base) (+ result 1))
        result))
  (iter n 0))

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))
(define (car z)
  (get-exponent-of-base 2 z))
(define (cdr z)
    (get-exponent-of-base 3 z))
