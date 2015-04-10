#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (pow10 x)
  (if (= x 0)
      1
      (* 10 (pow10 (- x 1)))))
  
(define (eightQueen)
  (define (isValid? x y)
    (if (or (or (= (car x) (car y)) (= (cdr x) (cdr y))) (= (abs (- (car x) (car y))) (abs (- (cdr x) (cdr y)))))
        #f
        #t))
  
  (define (check-ele lis now)
    (if (= now (- (length lis) 1))
        #t
        (and (isValid? (cons now (list-ref lis now)) (cons (- (length lis) 1) (list-ref lis (- (length lis) 1)))) (check-ele lis (+ now 1)))))
  
  (define (check? lis)
    (if (= (length lis) 1)
        #t
        (check-ele lis 0)))
  (define (hehe lis idx)
    (if (= idx 7)
        (list-ref lis 7)
        (+ (* (pow10 (- 7 idx)) (list-ref lis idx)) (hehe lis (+ idx 1)))))
           
  (define (numrize lis)
    (hehe lis 0))
 
  (define (gene-branch lis)
    (map (lambda (x) (append lis (list x))) (enumerate-interval 1 8)))
 
  (define (solve n lis)
    (if (= n 8)
        (map numrize (filter check? lis))
        (solve (+ n 1) (filter check? (flatmap gene-branch lis)))))
  
  (solve 0 (make-list 1 '())))
        
(define (myLoop k x lis)
  (if (= k 0)
      (void)
      (begin (display (list-ref lis x)) 
             (newline) 
             (myLoop (- k 1) (read) lis)))) 

(define (myWork)
  (let ((t (read)) (lis (eightQueen)))
    (myLoop t (read) lis)))

(myWork)

       