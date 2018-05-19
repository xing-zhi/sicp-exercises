(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (fringe items)
  (if (null? items)
      '()
      (let ((first-item (car items))
            (rest-items (cdr items)))
        (if (pair? first-item)
            (append (fringe first-item) (fringe rest-items))
            (cons first-item (fringe rest-items))))))

(define x (list (list 1 2) (list 3 4)))
