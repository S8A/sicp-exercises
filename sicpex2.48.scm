(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (make-segment start end) (list start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cadr seg))
