#lang racket
(define (myPrint vec n m)
  (if (<= m n) 
      (begin(display (vector-ref vec m))(display " ")(myPrint vec n (+ m 1)))
      (newline)))

(define (myLoop vec n m k)
  (if (< n k)
      (if (= m 0)
          (begin(vector-set! vec 0 1)(myPrint vec n 0)(myLoop vec (+ n 1) (+ n 1) k))
          (begin(vector-set! vec m (+ (vector-ref vec m) (vector-ref vec (- m 1))))(myLoop vec n (- m 1) k)))
      (void))
)

(define (myRead) (let ((x (read)))
  (if (eq? x eof)
      (void)
      (begin(myLoop (make-vector x) 0 0 x)(myRead)))))

(myRead)