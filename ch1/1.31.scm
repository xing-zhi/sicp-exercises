;;; product procedure generates a iterative process
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter (term a) 1))

(define (identity a) a)
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
(define (square a) (* a a))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (term k)
    (/ (* (dec k) (inc k))
       (square k)))
  (define (next a) (+ a 2))
  (if (even? n)
      (error "n must be odd")
      (* 4.0
         (product term 3 next n))))

;;; product procedure generates a recursive process
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))
