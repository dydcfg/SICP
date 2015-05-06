#lang scheme/gui


;Vector related

(define (make-vect x y) (list x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (length v)
  (sqrt (+ (* (xcor-vect v) (xcor-vect v))  (* (ycor-vect v) (ycor-vect v)))))

(define (sinθ v)
  (/ (ycor-vect v) (length v)))

(define (cosθ v)
  (/ (xcor-vect v) (length v)))

(define (rotation-vect v θ)
  (let ((x (xcor-vect v))
        (y (ycor-vect v)))
    (make-vect (- (* x (cos θ)) (* y (sin θ)))
               (+ (* x (sin θ)) (* y (cos θ))))))


;Frame related
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;定义一些画刷
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define red-pen (make-object pen% "RED" 2 'solid))
(define black-pen (make-object pen% "BLACK" 2 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-brush (make-object brush% "RED" 'solid))

;定义图形
(define (draw-face dc)
  (send dc set-smoothing 'smoothed)
  (send dc set-pen black-pen)
  (send dc set-brush no-brush)
  (send dc draw-ellipse 50 50 100 100)
  (send dc set-brush yellow-brush)
  (send dc draw-line 70 100 90 100)
  (send dc draw-ellipse 50 90 20 20)
  (send dc draw-ellipse 90 90 20 20)
  (send dc set-brush no-brush)
  (send dc set-pen red-pen)
  (let ([-pi (atan 0 -1)])
    (send dc draw-arc 50 60 60 80 (* 3/2 -pi) (* 7/4 -pi))))

;定义一个窗口
(define myWindow (new frame% [label "example window"] 
                   [width 300] [height 300]))

;定义一个面板,附着在刚才的窗口上
(define myCanvas (new canvas% 
                      [parent myWindow]
                      ;事件处理，Paint回调时将draw-face
                      [paint-callback (lambda (canvas dc) (draw-face dc))]))

(send myWindow show #t)