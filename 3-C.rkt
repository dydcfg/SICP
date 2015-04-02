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


(define (tri-num-list n s)
  (define (Correct? tri) (= s (+ (list-ref tri 0) (+ (list-ref tri 1) (list-ref tri 2)))))
    
  (define (find-all-triNum-and-cons num)
    (lambda (upper)
      (if (= num 1)
          (list (list upper))
          (map
           (lambda (x) (append (list upper) x))
           (flatmap (lambda (x)
                      ((find-all-triNum-and-cons (- num 1)) x))
                    (enumerate-interval (+ upper 1) (- n (- num 2))))))))
          
  (filter Correct? (flatmap (find-all-triNum-and-cons 3) (enumerate-interval 1 (- n 2)))))
    
        

(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)