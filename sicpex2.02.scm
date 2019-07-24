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
