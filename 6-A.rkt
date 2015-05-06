#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '(define (check-cycle lst)
         (define (check-cycle-itrt t lst)
           (if (null? t)
               #f
               (if (eq? lst t)
                   #t
                   (if (null? (cdr t))
                       #f
                       (check-cycle-itrt (cddr t) (cdr lst))))))
         (if (null? lst)
             #f
             (check-cycle-itrt (cdr lst) lst)))
         
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