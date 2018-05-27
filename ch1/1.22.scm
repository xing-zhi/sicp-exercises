(define (smallest-divisor n)
  (define (divides? a b) (= (remainder a b) 0))
  (define (try test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (try (+ test-divisor 1)))))
  (try 2))
(define (prime? n)
  (= (smallest-divisor n) n))

;;; 因为当前使用的Scheme的runtime的时间精度不在是微秒
;;; 所以使用毫秒时间精度的real-time-clock重新定义运行时间度量函数
(define (get-runtime f)
  (let ((start-time (real-time-clock)))
    (f)
    (- (real-time-clock) start-time)))

(define (search-for-primes start n)
  (define (next n)
    (if (even? n)
        (+ n 1)
        (+ n 2)))
  (cond ((> n 0)
         (cond ((prime? start)
                (newline)
                (display start)
                (search-for-primes (next start) (- n 1)))
               (else (search-for-primes (next start) n))))))

;;; 因为使用更小的数得到的运行时间都是0，所以使用10000000、100000000和1000000000
(search-for-primes 10000000 3)
;;; =>
;;; 10000019
;;; 10000079
;;; 10000103

(search-for-primes 100000000 3)
;;; =>
;;; 100000007
;;; 100000037
;;; 100000039
(search-for-primes 1000000000 3)
;;; =>
;;; 1000000007
;;; 1000000009
;;; 1000000021

(get-runtime (lambda () (prime? 10000019)))
;;; 3
(get-runtime (lambda () (prime? 10000079)))
;;; 4
(get-runtime (lambda () (prime? 10000103)))
;;; 4

(get-runtime (lambda () (prime? 100000007)))
;;; 10
(get-runtime (lambda () (prime? 100000037)))
;;; 11
(get-runtime (lambda () (prime? 100000039)))
;;; 11

(get-runtime (lambda () (prime? 1000000007)))
;;; 32
(get-runtime (lambda () (prime? 1000000009)))
;;; 32
(get-runtime (lambda () (prime? 1000000021)))
;;; 33

(define (average x y z) (/ (+ x y z) 3.0))

(define first-average-time (average 3 4 4))
(define second-average-time (average 10 11 11))
(define third-average-time (average 32 32 33))

(/ second-average-time first-average-time)
;;; => 2.91
(/ third-average-time second-average-time)
;;; => 3.03
(sqrt 10)
;;; => 3.16

;;; 从实际运行时间的比率来看，没有严格遵循√10的比率
;;; 后两个数量级的数据的运行时间可以看作是符合√n增长率的
;;; 从实际运行时间来看，运行时间和需要的运算数量并不严格成比例，这应该是因为不同运算的耗时不同
