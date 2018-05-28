(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(define (divide? a b) (= (remainder a b) 0))

(define e
  (+
   (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (if (divide? (+ i 1) 3)
                    (* 2 (/ (+ i 1) 3))
                    1))
              10)
   2.0)
  )

e
;;; => 2.7182817182817183
