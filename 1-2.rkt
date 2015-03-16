#lang racket
(define (myLoop n)
  (define (getMax m t)
    (if (> m 0)
      (getMax 
        (- m 1) 
        (let ((a (read)))
          (if (> a t)
            a
            t
          )  
        )
      )    
      t 
    )
  )
  (define (printMax) 
    (let ((m (read))(t (read)))
      (begin 
        (display (getMax (- m 1) t))
        (newline)
      )
    )
  )
  (if (> n 0)
    (begin (printMax)(myLoop (- n 1)))
    (void)
  )
)


(let ((n (read))) (myLoop n))
