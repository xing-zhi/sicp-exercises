(define (pascal-triangle n)
  (define (pascal-triangle-row row)
    (define (pascal-triangle-item row column)
      (if (or (= row 1) (= column 1) (= row column))
          1
          (+ (pascal-triangle-item (- row 1) (- column 1))
             (pascal-triangle-item (- row 1) column))))
    (define (column-iter column)
      (display (pascal-triangle-item row column))
      (display '" ")
      (if (< column row)
          (column-iter (+ column 1))))
    (column-iter 1))
  (define (row-iter row)
    (newline)
    (pascal-triangle-row row)
    (if (< row n)
        (row-iter (+ row 1))))
  (row-iter 1))
