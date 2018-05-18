(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (sign (if (< d 0)
                  -1
                  1)))
    (cons (* sign (/ n g)) (* sign (/ d g)))))
