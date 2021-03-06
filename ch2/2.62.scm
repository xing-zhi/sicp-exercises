(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((current-of-s1 (car s1))
                    (current-of-s2 (car s2))
                    (rest-of-s1 (cdr s1))
                    (rest-of-s2 (cdr s2)))
                (cond ((= current-of-s1 current-of-s2)
                       (cons current-of-s1 (union-set rest-of-s1 rest-of-s2)))
                      ((< current-of-s1 current-of-s2)
                       (cons current-of-s1 (union-set rest-of-s1 s2)))
                      (else (cons current-of-s2 (union-set s1 rest-of-s2))))))))
