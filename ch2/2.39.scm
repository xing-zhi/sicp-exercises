(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-with-fold-right sequence)
  (fold-right (lambda (item acc) (append acc (list item)))
              '()
              sequence))

(define (reverse-with-fold-left sequence)
  (fold-left (lambda (acc item) (cons item acc))
             '()
             sequence))
