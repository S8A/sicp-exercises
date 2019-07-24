(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (good_enough? prev guess x)
    (< (abs (/ (- guess prev) prev)) 0.001))

(define (sqrt_iter prev guess x)
    (if (good_enough? prev guess x)
        guess
        (sqrt_iter guess (improve guess x) x)))

(define (sq_rt x) (sqrt_iter -1.0 1.0 x))