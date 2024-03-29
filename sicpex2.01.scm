(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (make_rat n d)
  (let ((sign (if (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0))) 1 -1))
        (g (gcd n d)))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add_rat x y)
  (make_rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub_rat x y)
  (make_rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul_rat x y)
  (make_rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div_rat x y)
  (make_rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal_rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print_rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
