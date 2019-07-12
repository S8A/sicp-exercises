(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (good_enough? guess x) (< (abs (- (square guess) x)) 0.001))

(define (sqrt_iter guess x)
    (if (good_enough? guess x)
        guess
        (sqrt_iter (improve guess x) x)))

(define (sq_rt x) (sqrt_iter 1.0 x))