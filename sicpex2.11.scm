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
    cond (((and (< x2 0) (< y2 0)) (make_interval (* x2 y2) (* x1 y1)))
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


#| Full explanation
For each interval there are three relevant cases: it's to the left of zero (L),
it's to the right of zero (R), or it's on both sides (C). With two intervals, 
there are nine cases:

x y:  Case                            Simplified                      Found
L L:  x1 < 0, x2 < 0, y1 < 0, y2 < 0  x2 < 0, y2 < 0                  yes
L C:  x1 < 0, x2 < 0, y1 < 0, y2 > 0  x2 < 0, y1 < 0, y2 > 0          yes
L R:  x1 < 0, x2 < 0, y1 > 0, y2 > 0  x2 < 0, y1 > 0                  yes
C L:  x1 < 0, x2 > 0, y1 < 0, y2 < 0  x1 < 0, x2 > 0, y2 < 0          yes
C C:  x1 < 0, x2 > 0, y1 < 0, y2 > 0  x1 < 0, x2 > 0, y1 < 0, y2 > 0  else
C R:  x1 < 0, x2 > 0, y1 > 0, y2 > 0  x1 < 0, x2 > 0, y1 > 0          yes
R L:  x1 > 0, x2 > 0, y1 < 0, y2 < 0  x1 > 0, y2 < 0                  yes
R C:  x1 > 0, x2 > 0, y1 < 0, y2 > 0  x1 > 0, y1 < 0, y2 > 0          yes
R R:  x1 > 0, x2 > 0, y1 > 0, y2 > 0  x1 > 0, y1 > 0                  yes

..:: LL ::..
x1 < 0, x2 < 0, y1 < 0, y2 < 0
p1 < p2 => x1*y1 < x1*y2 => y1 > y2 => False => p2 < p1
p3 < p4 => x2*y1 < x2*y2 => y1 > y2 => False => p4 < p3
p1 < p3 => x1*y1 < x2*y1 => x1 > x2 => False => p3 < p1
p2 < p4 => x1*y2 < x2*y2 => x1 > x2 => False => p4 < p2
:. p4 < p3 < p1 and p4 < p2 < p1

..:: LC ::..
x1 < 0, x2 < 0, y1 < 0, y2 > 0
p1 < p2 => x1*y1 < x1*y2 => y1 > y2 => False => p2 < p1
p3 < p4 => x2*y1 < x2*y2 => y1 > y2 => False => p4 < p3
p1 < p3 => x1*y1 < x2*y1 => x1 > x2 => False => p3 < p1
p2 < p4 => x1*y2 < x2*y2 => x1 < x2 => True => p2 < p4
:. p2 < p4 < p3 < p1

..:: LR ::..
x1 < 0, x2 < 0, y1 > 0, y2 > 0
p1 < p2 => x1*y1 < x1*y2 => y1 > y2 => False => p2 < p1
p3 < p4 => x2*y1 < x2*y2 => y1 > y2 => False => p4 < p3
p1 < p3 => x1*y1 < x2*y1 => x1 < x2 => True => p1 < p3
p2 < p4 => x1*y2 < x2*y2 => x1 < x2 => True => p2 < p4
:. p2 < p4 < p3 and p2 < p1 < p3

..:: CL ::..
x1 < 0, x2 > 0, y1 < 0, y2 < 0
p1 < p2 => x1*y1 < x1*y2 => y1 > y2 => False => p2 < p1 
p3 < p4 => x2*y1 < x2*y2 => y1 < y2 => True => p3 < p4
p1 < p3 => x1*y1 < x2*y1 => x1 > x2 => False => p3 < p1
p2 < p4 => x1*y2 < x2*y2 => x1 > x2 => False => p4 < p2
:. p3 < p4 < p2 < p1

..:: CC ::..
x1 < 0, x2 > 0, y1 < 0, y2 > 0
p1 < p2 => x1*y1 < x1*y2 => y1 > y2 => False => p2 < p1
p3 < p4 => x2*y1 < x2*y2 => y1 < y2 => True => p3 < p4
p1 < p3 => x1*y1 < x2*y1 => x1 > x2 => False => p3 < p1
p2 < p4 => x1*y2 < x2*y2 => x1 < x2 => True => p2 < p4
:. Inconclusive

..:: CR ::..
x1 < 0, x2 > 0, y1 > 0, y2 > 0
p1 < p2 => x1*y1 < x1*y2 => y1 > y2 => False => p2 < p1
p3 < p4 => x2*y1 < x2*y2 => y1 < y2 => True => p3 < p4
p1 < p3 => x1*y1 < x2*y1 => x1 < x2 => True => p1 < p3
p2 < p4 => x1*y2 < x2*y2 => x1 < x2 => True => p2 < p4
:. p2 < p1 < p3 < p4

..:: RL ::..
x1 > 0, x2 > 0, y1 < 0, y2 < 0
p1 < p2 => x1*y1 < x1*y2 => y1 < y2 => True => p1 < p2
p3 < p4 => x2*y1 < x2*y2 => y1 < y2 => True => p3 < p4
p1 < p3 => x1*y1 < x2*y1 => x1 > x2 => False => p3 < p1
p2 < p4 => x1*y2 < x2*y2 => x1 > x2 => False => p4 < p2
:. p3 < p4 < p2 and p3 < p1 < p2

..:: RC ::..
x1 > 0, x2 > 0, y1 < 0, y2 > 0
p1 < p2 => x1*y1 < x1*y2 => y1 < y2 => True => p1 < p2
p3 < p4 => x2*y1 < x2*y2 => y1 < y2 => True => p3 < p4
p1 < p3 => x1*y1 < x2*y1 => x1 > x2 => False => p3 < p1
p2 < p4 => x1*y2 < x2*y2 => x1 < x2 => True => p2 < p4
:. p3 < p1 < p2 < p4

..:: RR ::..
x1 > 0, x2 > 0, y1 > 0, y2 > 0
p1 < p2 => x1*y1 < x1*y2 => y1 < y2 => True => p1 < p2
p3 < p4 => x2*y1 < x2*y2 => y1 < y2 => True => p3 < p4
p1 < p3 => x1*y1 < x2*y1 => x1 < x2 => True => p1 < p3
p2 < p4 => x1*y2 < x2*y2 => x1 < x2 => True => p2 < p4
:. p1 < p2 < p4 and p1 < p3 < p4
|#



