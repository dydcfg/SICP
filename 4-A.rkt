#lang racket

;(define (qsort lst)
;  (if (eq? '() lst)
;      '()
;      (append (qsort (filter (lambda (x)
;                               (< (cadr x) (cadar lst))) lst))
;              (list (car lst))
;              (qsort (filter (lambda(x)
;                               (> (cadr x) (cadar lst))) lst)))))


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


(define (generate-huffman-tree lst)
  (define (mywork)
    (if (= 1 (length huffman-tree))
        (void)
        (begin
          (set! huffman-tree (adjoin-set (make-code-tree (car huffman-tree) (cadr huffman-tree)) (cddr huffman-tree)))
          (mywork))))
  
  (set! huffman-tree (make-leaf-set lst))
  (mywork)
  huffman-tree)




(define (encode s myTree)
  (define (findCode x myTree)
    (if (leaf? myTree)
        '()
        (if (not (eq? '() (filter (lambda (y) (eq? x (string-ref (symbol->string y) 0))) (symbols (left-branch myTree)))))
            (append '(0) (findCode x (left-branch myTree)))
            (append '(1) (findCode x (right-branch myTree))))))
  
  (define (hehe x)
    (findCode x myTree))
    
  (define haha (flatmap hehe (string->list (symbol->string s))))
  haha)
            
  

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left ) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols  tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)))

  
  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit " bit))))


(define (adjoin-set x set)
;  (display "in adjoin-set:" ) (display "x=") (display x) (display "  set=" ) (display set) (newline);addfor debug
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
;  (display "in make-leaf-set:" ) (display pairs) (newline) ;addfor debug
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;(define (my-number->list num)
;  (if (< num 10)
;      (cons num '())
;      (append (my-number->list (floor (/ num 10))) (list (remainder num 10)))))
;(define tmp '((A 10000000) (B 1000000) (C 100000) (D 10000) (E 1000) (F 100) (G 10) (H 1)))
;(define tmptmp (make-leaf-set tmp))
;(define mytree (generate-huffman-tree tmp))
;(encode 'ABEFG mytree)




(define huffman-tree '())


(define (myloop) 
  (define (display-list lst)
    (if (null? lst)
        (void)
        (begin (display (car lst)) (display-list (cdr lst)))))

  (let ((a (read)))
     (if (eq? a eof)
         (void)
         (cond ((eq? a 'B) 
                (set! huffman-tree (generate-huffman-tree (read))) (myloop))
               ((eq? a 'E) 
                (display-list (decode (encode (read) huffman-tree) huffman-tree))
                (newline)
                (myloop))))))


(myloop)
;(generate-huffman-tree '((A 5) (B 4) (C 3) (E 2)))