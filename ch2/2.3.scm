(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (same-point? p1 p2)
  (and (= (x-point p1) (x-point p2))
       (= (y-point p1) (y-point p2))))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (segment-length segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (sqrt (+ (square (- (x-point end) (x-point start)))
             (square (- (y-point end) (y-point start)))))))

;;; 两条平行的线段是否方向相同
(define (segment-same-direction? segment1 segment2)
  (let ((start1 (start-segment segment1))
        (start2 (start-segment segment2))
        (end1 (end-segment segment1))
        (end2 (end-segment segment2)))
    (and (= (- (y-point end1) (y-point start1))
            (- (y-point end2) (y-point start2)))
         (= (- (x-point end2) (x-point start1))
            (- (x-point end2) (x-point start2))))))

;;; 矩形由4条边组成，而且满足对边平行，邻边垂直关系，所以可以通过2条边定义矩形
;;; 所以constructor可以是使用2条边构建矩形的函数
;;; 要计算矩形的周长和面积，都只需要矩形的长和宽
;;; 所以selectors是获取矩形的长和宽的函数：rectangle-length/rectangle-width
(define (rectangle-perimeter r)
  (let ((length (rectangle-length r))
        (width (rectangle-width r)))
    (* 2 (+ length width))))
(define (rectangle-area r)
  (let ((length (rectangle-length r))
        (width (rectangle-width r)))
    (* length width)))

;;; 使用2条垂直的线段，即一条长边和一条短边
;; (define (make-rectangle l w)
;;   (cons l w))
;; (define (rectangle-length r)
;;   (let ((l (car r)))
;;     (segment-length l)))
;; (define (rectangle-width r)
;;   (let ((w (cdr r)))
;;     (segment-length w)))

;;; 使用2条平行的线段，即两条长边或者短边，这里使用长边
;; (define (make-rectangle l1 l2)
;;   (cons l1 l2))
;; (define (get-rectangle-length r)
;;   (let ((l (car r)))
;;     (segment-length l)))
;; (define (get-rectangle-width r)
;;   (let ((l1 (car r))
;;         (l2 (cdr r))
;;         (w (make-segment (start-segment l1) (start-segment l2))))
;;     (segment-length w)))

;;; 以上两种矩形的表现形式，对于矩形的周长和面积的计算都没有影响
;;; 不过构造函数有差异，需要去确定是长边还是短边
;;; 从上面的两种表现形式也可以看出，指定任意两条边就可以确定一个矩形
;;; 通用的constructor是接受任意两条边作为参数，然后转化为长边和短边存储
;;; 这种实现方式对constructor的使用者和获取长度/宽度的函数都更加友好
(define (make-rectangle segment1 segment2)
  (let ((len1 (segment-length segment1))
        (len2 (segment-length segment2))
        (start1 (start-segment segment1))
        (start2 (start-segment segment2))
        (end1 (end-segment segment1))
        (end2 (end-segment segment2)))

    (if (or (same-point? start1 start2)
            (same-point? start1 end2)
            (same-point? end1 start2)
            (same-point? end1 end2))
        ;;; 邻边
        (if (> len1 len2)
            (cons segment1 segment2)
            (cons segment2 segment1))
        ;;; 平行边
        (let ((segment3
               (if (segment-same-direction? segment1 segment2)
                   (make-segment start1 start2)
                   (make-segment start1 end2))))
              (define len3 (segment-length segment3))
          (if (> len1 len3)
              (cons segment1 segment3)
              (cons segment3 segment1))))))
(define (rectangle-length r)
  (segment-length (car r)))
(define (rectangle-width r)
  (segment-length (cdr r)))
