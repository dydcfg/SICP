#lang racket
(define (revTree lis)
  (define (revBuild lis)
    (if (eq? (cdr lis) '())
        lis
        (append (revBuild (cdr lis)) (list (car lis)))))
  (if (eq? lis '())
      lis
      (if (list? lis)
          (revBuild (map revTree lis))
          lis)))
(define (myRead)
  (let ((lis (read)))
    (if (eq? lis eof)
        (void)
        (begin 
          (display (revTree lis))
          (newline)
          (myRead)))))
(myRead)