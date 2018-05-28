(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (define (iter result i)
    (if (= i 1)
        result
        (iter (compose f result) (- i 1))))
  (iter f n))

((repeated square 2) 5)
;;; => 625
