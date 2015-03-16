#lang racket

(define (myLoop base now remain)
  (if (= remain 0)
      (begin(display now)(newline))
      (if (odd? remain)
                (myLoop (* base base) (* now base) (floor(/ remain 2)))
                (myLoop (* base base) now (floor(/ remain 2))))))

(define (myRead) (let ((x (read)) (y (read)))
  (if (eq? x eof)
      (void)
      (begin(myLoop x 1 y)(myRead)))))

(myRead)