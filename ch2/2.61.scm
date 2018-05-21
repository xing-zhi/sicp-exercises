(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((current (car set))
            (rest (cdr set)))
        (cond ((= current x) set)
              ((< current x) (cons current (adjoin-set x rest)))
              (else (cons x set))))))

;;; 操作需要的步骤和插入的数在结果结合中的位置相同，出现在任意位置概率相同时需要的平均步骤为`'(/ (+ 1 ... n) n)`即`'(/ (/ (* (+ 1 n) n) 2) n)`，最终结果为`(/ (+ 1 n) 2)`
