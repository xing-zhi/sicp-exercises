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

(define (dot-product v m)
  (accumulate + 0 (map * v m)))

(define (matrix-*-vector m v)
  (map (lambda (col) (dot-product col v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (col) (matrix-*-vector cols col)) m)))
