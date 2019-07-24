; From exercise 2.2.
(define (make_segment start end) (cons start end))
(define (start_segment seg) (car seg))
(define (end_segment seg) (cdr seg))

(define (make_point x y) (cons x y))
(define (x_point p) (car p))
(define (y_point p) (cdr p))

(define (print_point p)
  (newline)
  (display "(")
  (display (x_point p))
  (display ",")
  (display (y_point p))
  (display ")"))

(define (average a b) (/ (+ a b) 2))

(define (midpoint_segment seg)
  (let ((start (start_segment seg))
        (end (end_segment seg)))
    (make_point (average (x_point start) (x_point end))
                (average (y_point start) (y_point end)))))


; Exercise 2.3.
(define (make_rectangle p1 p2) (cons p1 p2))
(define (start_rect r) (car r))
(define (end_rect r) (cdr r))
(define (ylen_rect r)
  (let ((p1y (y_point (start_rect r)))
        (p2y (y_point (end_rect r))))
    (abs (- p2y p1y))))
(define (xlen_rect r)
  (let ((p1x (x_point (start_rect r)))
        (p2x (x_point (end_rect r))))
    (abs (- p2x p1x))))

(define (rect_perimeter r)
  (+ (* 2 (xlen_rect r)) (* 2 (ylen_rect r))))

(define (rect_area r)
  (* (xlen_rect r) (ylen_rect r)))


(define (make_recc p1 p2) (make_segment p1 p2))
