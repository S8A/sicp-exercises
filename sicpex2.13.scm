; From exercise 2.11.
(define (make_interval a b) (cons a b))
(define (lower_bound interval) (car interval))
(define (upper_bound interval) (cdr interval))

(define (add_interval x y)
  (make_interval (+ (lower_bound x) (lower_bound y))
                 (+ (upper_bound x) (upper_bound y))))

(define (sub_interval x y)
  (add_interval x
                (make_interval (* -1 (lower_bound y))
                               (* -1 (upper_bound y)))))

(define (mul_interval x y)
  (let ((x1 (lower_bound x))
        (x2 (upper_bound x))
        (y1 (lower_bound y))
        (y2 (upper_bound y)))
    (cond ((and (< x2 0) (< y2 0)) (make_interval (* x2 y2) (* x1 y1)))
         ((and (< x2 0) (< y1 0) (> y2 0)) (make_interval (* x1 y2) (* x1 y1)))
         ((and (< x2 0) (> y1 0)) (make_interval (* x1 y2) (* x2 y1)))
         ((and (< x1 0) (> x2 0) (< y2 0)) (make_interval (* x2 y1) (* x1 y1)))
         ((and (< x1 0) (> x2 0) (> y1 0)) (make_interval (* x1 y2) (* x2 y2)))
         ((and (> x1 0) (< y2 0)) (make_interval (* x2 y1) (* x1 y2)))
         ((and (> x1 0) (< y1 0) (> y2 0)) (make_interval (* x2 y1) (* x2 y2)))
         ((and (> x1 0) (> y1 0)) (make_interval (* x1 y1) (* x2 y2)))
         (else (make_interval (min (* x1 y1) (* x1 y2) (* x2 y1) (* x2 y2))
                               (max (* x1 y1) (* x1 y2) (* x2 y1) (* x2 y2))))
         )))

(define (div_interval x y)
  (let ((uy (upper_bound y))
        (ly (lower_bound y)))
    (if (or (= uy 0) (= ly 0))
        (error "Dividing by zero is impossible.")
        (mul_interval x (make_interval (/ 1.0 uy) (/ 1.0 ly))))))


(define (make_center_width c w)
  (make_interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower_bound i) (upper_bound i)) 2))
(define (width i)
  (/ (- (upper_bound i) (lower_bound i)) 2))


; From exercise 2.12.
(define (make_center_percent c p)
  (make_center_width c (/ (* c p) 100.0)))

(define (percent i)
  (* 100 (/ (- (upper_bound i) (center i)) (center i))))


; Exercise 2.13
#| Assume there are two positive intervals x±a and y±b. Applying mul_interval 
to them would use the formula for the case x1>0 and y1>0, so the resulting 
interval would be (x1*y1, x2*y2). Replacing the values we get:
x1*y1 = (x-a)(y-b) = xy-bx-ay+ab = xy+ab-(bx+ay)
x2*y2 = (x+a)(y+b) = xy+bx+ay+ab = xy+ab+(bx+ay)
As a->0 and b->0, both x1*y1 and x2*y2 approach xy.

I don't understand the implications of this.
|#
