#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现,不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-polynomial-package)
  (define (tag x)
    (attach-tag 'polynomial x))  
  
  (define (empty-termlist? p)
    (null? p))
  
  (define (add-terms t1 t2)
        (cond ((empty-termlist? t1) t2)
              ((empty-termlist? t2) t1)
              (else
               (let ((x1 (car t1)) (x2 (car t2)))
                 (cond ((> (order x1) (order x2))
                        (append (list x1) (add-terms (cdr t1) t2)))
                       ((< (order x1) (order x2))
                        (append (list x2) (add-terms t1 (cdr t2))))
                       (else
                        (append (list (add x1 x2)) (add-terms (cdr t1) (cdr t2)))))))))
  
  (define (add-term-const p1 p2)
    (cond ((empty-termlist? p1)
           (list p2))
          ((empty-termlist? p2)
           (list p1))
          (else
           (if (= 0 (order (car p1)))
               (list (add (car p1) p2))
               (append (car p1) (add-term-const (cdr p1) p2))))))
  
  
  (define (add-poly p1 p2)
    (cond ((eq? (rank (cadr p1)) (rank (cadr p2)))
           (append (list  (cadr p1)) (add-terms (cddr p1) (cddr p2))))
          ((> (rank (cadr p1)) (rank (cadr p2)))
           (list  (cadr p1) (add-term-const (cddr p1) p2)))
          ((< (rank (cadr p1)) (rank (cadr p2)))
           (list  (cadr p2) (add-term-const (cddr p2) p1)))))
     
  (define (add-poly-int p1 p2)
    (append (list (car p1) (cadr p1)) add-term-const (cddr p1) p2))
     
  
  (define (mul-terms p1 p2)
    (if (empty-termlist? p1)
        '()
        (add-terms (mul-term-by-all-terms (car p1) p2)
                   (mul-terms (cdr p1) p2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        '()
        (let ((t2 (car L)))
          (append (list (mul t1 t2))
                  (mul-term-by-all-terms t1 (cdr L))))))
  
  (define (mul-term-const p1 p2)
    (cond ((empty-termlist? p1)
           '())
          ((empty-termlist? p2)
           '())
          (else
           (append (list (list (caar p1) (mul (cdar p1) p2))) (mul-term-const (cdr p1) p2)))))
  
  (define (mul-poly p1 p2)
    (cond ((eq? (rank (cadr p1)) (rank (cadr p2)))
           (append (list (cadr p1)) (mul-terms (cddr p1) (cddr p2))))
          ((> (rank (cadr p1)) (rank (cadr p2)))
           (list  (cadr p1) (mul-term-const (cddr p1) p2)))
          ((< (rank (cadr p1)) (rank (cadr p2)))
           (list  (cadr p2) (mul-term-const (cddr p2) p1)))))
  
  (define (mul-my-int p1 p2)
    (list (+ (car p1) (car p2)) (cons 'integer (* (cdadr p1) (cdadr p2)))))
  
  
  (define (add-my-int p1 p2)
    (list (car p1) (cons 'integer (+ (cdadr p1) (cdadr p2)))))
  
  (put 'make 'polynomial
       (lambda (x y) (tag (cons x y))))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'add '(myinteger myinteger)
       (lambda (p1 p2) (add-my-int p1 p2)))
  (put 'add '(myinteger polynomial)
       (lambda (p1 p2) (tag (add-poly-int p2 p1))))
  (put 'add '(polynomial myinteger)
       (lambda (p1 p2) (tag (add-poly-int p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(myinteger myinteger)
       (lambda (p1 p2) (mul-my-int p1 p2)))
  ;(put 'mul '())
  (void))
  
 
(define (order x)
  (car x))

(define (rank x)
  (cond ((eq? x 'a) 5)
        ((eq? x 'b) 4)
        ((eq? x 'c) 3)
        ((eq? x 'd) 2)
        ((eq? x 'e) 1)
        (else
         0)))
                       
(define (type-tag-new exp)
  (if (eq? (car exp) 'polynomial)
      'polynomial
      'myinteger))
                        
(define (apply-generic op . args)
  (let ((type-tags (map type-tag-new args)))
    (let ((proc (get op type-tags)))
      (apply proc args))))


(define (parse-term exp)
  (if (eq? exp '())
      '()
      (let ((a (car exp))
            (b (cdr exp)))
        (if (eq? (cadr a) 'integer)
            (append (list (list (car a) (cddr a))) (parse-term b))
            (append (list (parse-poly a)) (parse-term b))))))

(define (parse-poly exp)
  (append (list (cadr exp)) (parse-term (cddr exp))))


(define (getType exp)
  (let ((x (car exp)))
    (if (or (eq? x 'a) (eq? x 'b) (eq? x 'c) (eq? x 'd) (eq? x 'e))
        'polynomial
        'integer)))

(define (build-term exp)
  (if (eq? exp '())
      '()
      (let ((a (car exp))
              (b (cdr exp)))
          (if (eq? (getType a) 'polynomial)
              (append (list (build-poly a)) (build-term exp))
              (append (list (list (car a) (cons 'integer (cdr a)))) (build-term exp))))))

(define (build-poly exp)
  (append (list 'polynomial (car exp)) (build-term (cdr exp))))


(define (display-poly exp)
  (displayln (parse-poly exp)))



(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
;(myloop)