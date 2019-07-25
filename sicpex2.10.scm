(define (make_interval a b) (cons a b))
(define (lower_bound interval) (car interval))
(define (upper_bound interval) (cdr interval))
(define (width x) (/ (abs (- (upper_bound x) (lower_bound x))) 2))

(define (add_interval x y)
  (make_interval (+ (lower_bound x) (lower_bound y))
                 (+ (upper_bound x) (upper_bound y))))

(define (sub_interval x y)
  (add_interval x
                (make_interval (* -1 (lower_bound y))
                               (* -1 (upper_bound y)))))

(define (mul_interval x y)
  (let ((p1 (* (lower_bound x) (lower_bound y)))
        (p2 (* (lower_bound x) (upper_bound y)))
        (p3 (* (upper_bound x) (lower_bound y)))
        (p4 (* (upper_bound x) (upper_bound y))))
    (make_interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div_interval x y)
  (let ((uy (upper_bound y))
        (ly (lower_bound y)))
    (if (or (= uy 0) (= ly 0))
        (error "Dividing by zero is impossible.")
        (mul_interval x (make_interval (/ 1.0 uy) (/ 1.0 ly))))))

