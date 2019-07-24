(define (square x) (* x x))

(define (sum_squares x y) (+ (square x) (square y)))

(define (sum_squares_largest_two a b c)
    (if (a >= b)
        (if (b >= c)
            (sum_squares a b)
            (sum_squares a c))
        (if (a >= c)
            (sum_squares a b)
            (sum_squares b c))))
