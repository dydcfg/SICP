#lang racket

(define (myPrint idx n vec)
  (if (< idx n)  
      (if (eq? (vector-ref vec idx) (vector-ref vec (- idx 1)))
          (myPrint (+ idx 1) n vec)
          (begin (display " ") (display (vector-ref vec idx)) (myPrint (+ idx 1) n vec))) ;(display "1")
      (newline)))

(define (mySort idx idy n vec)
  (if (< idx n)
      (if (< idy n)
          ((lambda()
            (if (< (vector-ref vec idy) (vector-ref vec idx))
                (let ((x (vector-ref vec idx)) (y (vector-ref vec idy)))
                  (begin (vector-set! vec idx y) 
                         (vector-set! vec idy x)))
                (void))
             (mySort idx (+ idy 1) n vec)))   
          (mySort (+ idx 1) (+ idx 1) n vec))
      ((lambda() (display (vector-ref vec 0)) (myPrint 1 n vec)))))

(define (myRead n vec)
  (let ((m (read)))
    (if (eq? m eof)
        (mySort 0 0 n vec)
        (begin (vector-set! vec n m) (myRead (+ n 1) vec)))))

(myRead 0 (make-vector 100))
