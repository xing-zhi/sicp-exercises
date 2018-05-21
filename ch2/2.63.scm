(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

;;; 得到的列表相同。以下是转换2.16中的树得到的结果。从结果可以看出，只要树由相同的元素组成，都得到相同的列表，和树的形状无关。这是二叉树中左分支的元素总是小于根结点的元素，右分支的元素总是大于根结点的元素这一特性决定的。

(define tree1 (make-tree 7
                         (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 '() (make-tree 11 '() '()))))
tree1
(tree->list-1 tree1)
;;; => (1 3 5 7 9 11)
(tree->list-2 tree1)
;;; => (1 3 5 7 9 11)

(define tree2 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9 '() (make-tree 11 '() '())))))
tree2
(tree->list-1 tree2)
;;; => (1 3 5 7 9 11)
(tree->list-2 tree2)
;;; => (1 3 5 7 9 11)

(define tree3 (make-tree 5
                         (make-tree 3 (make-tree 1 '() '()) '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))
tree3
(tree->list-1 tree3)
;;; => (1 3 5 7 9 11)
(tree->list-2 tree3)
;;; => (1 3 5 7 9 11)

;;; tree->list-1要执行n次(append + cons)操作，其中append的时间复杂度是O(n)，cons的时间复杂度是O(1)，因此tree->list-1的时间复杂度为O(n^2)
;;; tree->list-2要执行n次cons，cons的时间复杂度为O(1)，因此tree->list-2的时间复杂度为O(n)
