(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

;;; 使用替换模型，car的执行如下：
;;; (car z) =>
;;; ((lambda (m) (m x y)) (lambda (p q) p)) =>
;;; ((lambda (p q) p) x y) => x
;;; 所以cdr定义如下
(define (cdr z)
  (z (lambda (p q) q)))
