#lang racket
(define (count-pairs lst)
    (let ((dict (list 1)) (cash 5))
      (define (test x)
        (if (= 0 x)
            (void)
            (begin
              (set! cash (+ cash 5))
              (test (- x 1)))))
      (test 3)
      (display cash)))

(count-pairs 1)