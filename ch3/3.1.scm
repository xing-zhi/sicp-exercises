(define (make-accumulator initial)
  (lambda (x)
          (begin (set! initial (+ initial x))
                 initial)))
