(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))
(define (union-set s1 s2)
  (if (null? s2)
      s1
      (union-set (cons (car s2) s1) (cdr s2))))
(define (intersection-set s1 s2)
  (define (iter s1 result)
    (if (null? s1)
        result
        (let ((first-of-s1 (car s1))
              (rest-of-s1 (cdr s1)))
          (if (element-of-set? first-of-s1 s2)
              (iter rest-of-s1 (cons first-of-s1 result))
              (iter rest-of-s1 result)))))
  (iter s1 '()))

;;; element-of-set?的时间复杂度是O(n)，和不可重复版本相同
;;; intersection-set的时间复杂度都是O(n^2)，和不可重复版本相同
;;; adjoin-set的复杂度为O(1)，优于元素不可重复版本的O(n)
;;; union-set的复杂度为O(n)，优于元素不可重复版本的O(n^2)
;;; 综上，adjoin-set和union-set的时间复杂度要低一个量级。elemnt-of-set?和intersection-set的时间复杂度相同，但是因为可重复版本空间复杂度更高，性能略差；而且随着程序的运行，因为重复元素不断增多，性能差距会不断变大。
;;; 如果应用对空间复杂度不敏感，同时有大量的adjoin-set和union-set操作，可以考虑使用不可重复版本。
