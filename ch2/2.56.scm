(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (let ((m1 (multiplier exp))
               (m2 (multiplicand exp)))
           (make-sum (make-product m1 (deriv m2 var))
                     (make-product m2 (deriv m1 var)))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (e (exponent exp)))
           (make-product e
                         (make-product
                          (make-exponentiation b (- e 1))
                          (deriv b var)))))
        (else (error "unknow exprssion type: DERIV" exp))))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x)
       (variable? y)
       (eq? x y)))

(define (sum? e)
  (and (pair? e)
       (eq? (car e) '+)))
(define (addend e)
  (cadr e))
(define (augend e)
  (caddr e))
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list '+ a1 a2))))

(define (product? e)
  (and (pair? e)
       (eq? (car e) '*)))
(define (multiplier e)
  (cadr e))
(define (multiplicand e)
  (caddr e))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) '**)))
(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e) (expt b e)))
        (else (list '** b e))))
