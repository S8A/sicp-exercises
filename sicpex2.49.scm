; From exercise 2.23.
(define (for_each proc items)
  (cond ((null? items) #t)
        (else (proc (car items)) (for_each proc (cdr items)))))

; From exercise 2.46.
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
         (edge1-frame frame))
         (scale-vect (ycor-vect v)
         (edge2-frame frame))))))

(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (add-vect v1 (make-vect (- (xcor-vect v2)) (- (ycor-vect v2)))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))


; From exercise 2.47.
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

; From exercise 2.48.
(define (make-segment start end) (list start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cadr seg))


; Exercise 2.49.
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; a
(define outline-painter
  (lambda (frame)
    (segments->painter
      (list (make-segment (origin-frame frame) (edge1-frame frame))
            (make-segment (origin-frame frame) (edge2-frame frame))
            (make-segment (edge1-frame frame)
                          (add-vect (edge1-frame frame) (edge2-frame frame)))
            (make-segment (edge2-frame frame)
                          (add-vect (edge1-frame frame) (edge2-frame frame))))
      frame)))

; b
(define x-painter
  (lambda (frame)
    (segments->painter
      (list (make-segment (origin-frame frame)
                          (add-vect (edge1-frame frame) (edge2-frame frame)))
            (make-segment (edge1-frame frame) (edge2-frame frame)))
      frame)))

; c
(define diamond-painter
  (lambda (frame)
    (let ((mid-edge1 (scale-vect 0.5 (edge1-frame frame)))
          (mid-edge2 (scale-vect 0.5 (edge2-frame frame))))
      (segments->painter 
        (list (make-segment mid-edge1 mid-edge2)
              (make-segment mid-edge2
                            (add-vect (edge2-frame frame) mid-edge1))
              (make-segment mid-edge1
                            (add-vect (edge1-frame frame) mid-edge2))
              (make-segment (add-vect (edge2-frame frame) mid-edge1)
                            (add-vect (edge1-frame frame) mid-edge2)))
        frame))))

; d
; To hell with the wave painter.
