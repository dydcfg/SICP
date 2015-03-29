#lang racket
(define (accumulate op initial sequence) (if (null? sequence)
        initial
        (op (car sequence)
(accumulate op initial (cdr sequence)))))

(define (myLoop)
  (define (flatIt) 
    (lambda (x) 
      (if (not (list? x))
          (list x)
          (if (eq? x '())
              x
              (if (list? (car x))
                  (append ((flatIt) (car x)) ((flatIt) (cdr x)))
                  (append (list (car x)) ((flatIt) (cdr x))))))))
              
  (define (flat-map lis)
    (accumulate append '() (map (flatIt) lis)))
  
  (let ((lis (read)))
    (if (eq? lis eof)
        (void)
        (begin(display (flat-map lis)) (newline) (myLoop)))))

(myLoop)