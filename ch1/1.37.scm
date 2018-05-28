(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
;;; => 0.6180555555555556
;;; 要得到小数点后4位的精度，k最少是11

(define (cont-frac-recursive n d k)
  (if (= k 0)
      1
      (/ (n k)
         (+ (d k)
            (cont-frac-recursive n d (- k 1))))))
