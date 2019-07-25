(define (add_interval x y)
  (make_interval (+ (lower_bound x) (lower_bound y))
                 (+ (upper_bound x) (upper_bound y))))

(define (mul_interval x y)
  (let ((p1 (* (lower_bound x) (lower_bound y)))
        (p2 (* (lower_bound x) (upper_bound y)))
        (p3 (* (upper_bound x) (lower_bound y)))
        (p4 (* (upper_bound x) (upper_bound y))))
    (make_interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div_interval x y)
  (mul_interval x 
                (make_interval (/ 1.0 (upper_bound y))
                               (/ 1.0 (lower_bound y)))))

(define (make_interval a b) (cons a b))


(define (lower_bound interval) (car interval))
(define (upper_bound interval) (cdr interval))
