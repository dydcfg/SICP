#lang racket
(define (cont-frac-iter N D k)
  (define (work ans n)
    (if (= n 0)
        ans
        (work (* (/ (N n) (+ (D n) ans)) 1.0) (- n 1))))
  (work 0 k))
  
  
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)