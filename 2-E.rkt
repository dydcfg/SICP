#lang racket

(define (divides? b a) 
(= (remainder b a) 0))
(define (f rdx base itrt product)
  (define (cal rdx base itrt ans product)
    (if (divides? product rdx)
        (if (divides? product base)
                  (cal rdx (floor (/ base rdx)) (floor (- itrt 1)) (+ ans itrt) (floor (/ product base)))
                  (cal rdx (floor (/ base rdx)) (floor (- itrt 1)) ans product))
        
         ans))
  (if (< base product)
      (f rdx (* base rdx) (+ itrt 1) product)
      (cal rdx base itrt 0 product)))
(define (car z)
  (f 2 2 1 z))
(define (cdr z)
  (f 3 3 1 z))

(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))



(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))


(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)