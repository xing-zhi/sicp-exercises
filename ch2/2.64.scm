(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;;; partial-tree把有序的list的转换为前n个数由前n个树组成的平衡树和剩余元素组成的列表的pair。
;;; 首先取前`(quotient (- n 1) 2)`（(/ (n - 1) 2)向下取整）个数递归调用partial-tree得到左子平衡树和剩余元素列表组成的pair。
;;; 然后从得到的pair中提取左子平衡树和剩余元素的列表，并通过元素总数减去左子平衡树包含的元素个数和根结点数量（1），用于构建右子平衡树的元素数量。
;;; 然后取下一个元素作为根元素。
;;; 然后使用除左子平衡树和根节点的元素外的元素和右平衡子树的元素数量递归调用partial-tree得到右子平衡树和剩余元素列表组成的pair。
;;; 然后从得到的pair中提取右子平衡树和剩余元素列表。
;;; 最后返回由根结点、左子平衡树、右子平衡树组成的树和剩余元素列表组成的pair。

(list->tree (list 1 3 5 7 9 11))
;;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;;;        5
;;;       / \
;;;      1   9
;;;      \  / \
;;;      3 7  11

;;; list->tree要执行n次make-tree，make-tree的时间复杂度是O(1)，所以list->tree的时间复杂度为O(n)。
