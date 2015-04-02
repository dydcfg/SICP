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
          (map (lambda (x) (list x upper)) (enumerate 1 (- upper 1)))
          (map
           (lamda (x) (append x (list upper)))
           (flatmap (lambda (x)
                      ((find-all-triNum-and-cons (- num 1)) x))
                    (enumerate 1 (- upper 1)))))))
          
  (filter Correct? (find-all-triNum n 3)))
    
        

(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)