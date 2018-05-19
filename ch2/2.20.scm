(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items '()))

(define (same-parity first-item . items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (if (equal? (even? first-item) (even? (car items)))
                  (cons (car items) result)
                  result))))
  (reverse (iter items (list first-item))))
