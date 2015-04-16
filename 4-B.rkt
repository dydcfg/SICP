#lang racket

(define (flatme exp)
  (if (list? exp)
      (if (eq? (cdr exp) '())
          (car exp)
          exp)
      exp))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? x y)
  (if (not (number? x))
      #f
      (= x y)))

(define (make-sum a1 a2) 
  (cond 
    ((=number? a1 0) a2) 
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2)) 
    (else
     (cond
       ((and (list? a1) (list? a2)) (append a1 (list '+) a2))
       ((list? a1) (append a1 (list '+ a2)))
       ((list? a2) (append (list a1 '+) a2))
       (else (list a1 '+ a2))))))
(define (make-product m1 m2)
  (cond 
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2)) 
    (else (list m1 '* m2))))
(define (sum? x)
  (not (eq? (filter (lambda (z) (eq? '+ z)) x) '())))
(define (addend-flat s)
  (if (eq? (car s) '+)
      '()
      (append (list (car s)) (addend-flat (cdr s)))))
(define (addend s)  
  (flatme (addend-flat s)))

(define (augend-flat s) 
  (if (eq? (car s) '+)
      (cdr s)
      (augend-flat (cdr s))))

(define (augend s) 
  (flatme (augend-flat s)))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) 
  (car p))
(define (multiplicand p)
  (flatme (cddr p)))

(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)   
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))

(myloop)