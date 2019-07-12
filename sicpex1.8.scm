(define (square x) (* x x))
(define (cube x) (* x x x))

(define (improve guess x)
	(/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good_enough? guess x) (< (abs (- (cube guess) x)) 0.001))

(define (curt_iter guess x)
    (if (good_enough? guess x)
        guess
        (curt_iter (improve guess x) x)))

(define (curt x) (curt_iter 1.0 x))