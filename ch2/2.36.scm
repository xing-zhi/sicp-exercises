(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op
                        init
                        (accumulate (lambda (x y) (cons (car x) y))
                                    '()
                                    seqs))
            (accumulate-n op
                          init
                          (accumulate (lambda (x y) (cons (cdr x) y))
                                      '()
                                      seqs)))))
