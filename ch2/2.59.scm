(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
  (if (null? s2)
      s1
      (let ((first-of-s2 (car s2))
            (rest-of-s2 (cdr s2)))
        (if (element-of-set? first-of-s2 s1)
            (union-set s1 rest-of-s2)
            (union-set (cons first-of-s2 s1) rest-of-s2)))))
