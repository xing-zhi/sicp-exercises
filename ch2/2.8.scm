(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (uppper-bound y))
                               (- (lower-bound y)))))
;;; sub-interval可以通过有被减数和由减数的上界的负数和减数的下界的负数组成的interval的和计算得到
