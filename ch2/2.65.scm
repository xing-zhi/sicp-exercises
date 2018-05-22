;;; tree的constructor和selectors
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;; list->tree，时间复杂度为O(n)
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

;;; tree->list，时间复杂度为O(n)
(define (tree->list tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

;;; 时间复杂度为O(n)且结果为平衡二叉树的union-set
;;; 依次调用3个时间复杂度为O(n)的函数：tree->list、union-set-list和list->tree
(define (union-set s1 s2)
  (define (union-set-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else
           (let ((current1 (car l1))
                 (current2 (car l2))
                 (rest1 (cdr l1))
                 (rest2 (cdr l2)))
             (cond ((= current1 current2)
                    (cons current1 (union-set-list rest1 rest2)))
                   ((> current1 current2)
                    (cons current2 (union-set-list l1 rest2)))
                   (else (cons current1 (union-set-list rest1 l2))))))))
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (let ((union-list (union-set-list l1 l2)))
      (list->tree union-set-list))))

;;; 时间复杂度为O(n)且结果为平衡二叉树的intersection-set
;;; 依次调用3个时间复杂度为O(n)的函数：tree->list、intersection-set-list和list->tree
(define (intersection-set s1 s2)
  (define (intersection-set-list l1 l2)
    (cond ((null? l1) '())
          ((null? l2) '())
          (else
           (let ((current1 (car l1))
                 (crrent2 (car l2))
                 (rest1 (cdr l1))
                 (rest2 (cdr l2)))
             (cond ((=current1 current2)
                    (cons (current1 (intersection-set-list rest1 rest2)))
                    ((> current1 current2)
                     (intersection-set-list l1 rest2))
                    (else intersection-set-list rest1 l2)))))))
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (let ((intersection-list (intersection-set-list l1 l2)))
      (list->tree intersection-set-list))))
