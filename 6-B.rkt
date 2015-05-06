#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '(define (count-pairs lst)
    (let ((dict '()))
      (define (check-exist x dict)
        (if (null? dict)
            #f
            (if (eq? (car dict) x)
                #t
                (check-exist x (cdr dict)))))
      (define (my-count-pairs x)
          (if (not (pair? x))
              0
              (if (check-exist x dict)
                  0
                  (begin
                    (set! dict (cons x dict))
                    (+ (my-count-pairs (car x))
                       (my-count-pairs (cdr x))
                       1)))))
      (my-count-pairs lst)))
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)