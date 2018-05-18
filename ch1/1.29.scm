(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc a) (+ a 1))

(define (cube a) (* a a a))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (item k)
    (define (factor x)
      (cond ((or (= k 0) (= k n)) 1)
            ((even? k) 2)
            (else 4)))
    (* (factor k)
       (f (+ a (* k h)))))
  (if (even? n)
      (* (/ h 3.0) (sum item 0 inc n))
      (error "n must be even.")))

(integral cube 0 1 100)
;;; => 0.25

(integral cube 0 1 1000)
;;; => 0.25
