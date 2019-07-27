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

; From the book
(define (par1 r1 r2)
  (div_interval (mul_interval r1 r2)
                (add_interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make_interval 1 1))) 
    (div_interval one
                  (add_interval (div_interval one r1)
                                (div_interval one r2)))))


; Exercise 2.14.
#| Assume there are two positive intervals A=x±a and B=y±b

(mul_interval A B) => (mul_interval x±a y±b)
=> (make_interval (x-a)(y-b) (x+a)(y+b))
=> ((x-a)(y-b), (x+a)(y+b))

(div_interval A B) => (div_interval x±a y±b)
=> (mul_interval (x-a,x+a) (1/(y+b),1/(y-b)))
=> (make_interval (x-a)/(y+b) (x+a)/(y-b))
=> ((x-a)/(y+b), (x+a)/(y-b))

(par1 A B) => (div_interval (mul_interval A B) (add_interval A B))
=> (div_interval ((x-a)(y-b), (x+a)(y+b)) (x-a+y-b, x+a+y+b))
=> (mul_interval ((x-a)(y-b), (x+a)(y+b)) (1/(x-a+y-b), 1/(x+a+y+b)))
=> ((x-a)(y-b)/(x-a+y-b), (x+a)(y+b)/(x+a+y+b))

(par2 A B)
=> (div_interval (1,1) (add_interval (div_interval (1,1) A) (div_interval (1,1) B)))
=> (div_interval (1,1) (add_interval (mul_interval (1,1) (1/(x-a),1/(x+a))) (mul_interval (1,1) (1/(y-b),1/(y+b)))))
=> (div_interval (1,1) (add_interval (1/(x-a),1/(x+a)) (1/(y-b),1/(y+b)))
=> (div_interval (1,1) (1/(x-a) + 1/(y-b), 1/(x+a) + 1/(y+b)))
=> (div_interval (1,1) ((y-b+x-a)/((x-a)(y-b)), (y+b+x+a)/((x+a)(y+b))))
=> (mul_interval (1,1) ((x-a)(y-b)/(y-b+x-a), (x+a)(y+b)/(y+b+x+a)))
=> ((x-a)(y-b)/(y-b+x-a), (x+a)(y+b))(y+b+x+a))
=> ((x-a)(y-b)/(x-a+y-b), (x+a)(y+b))(x+a+y+b))

(par1 A B) and (par2 A B) are theoretically equal. However, computers work with 
digital data, so numbers have limited decimal precision. For sufficiently small 
intervals, there might be some truncation and/or rounding errors, specially 
with par2, since it requires more operations.

Empirical testing ::..
1 ]=> (define a (make_center_percent 10 0.5))
1 ]=> (define b (make_center_percent 7 1))
1 ]=> (par1 a b)
;Value 13: (4.027657710280374 . 4.20933056872038)
1 ]=> (par2 a b)
;Value 14: (4.08492298578199 . 4.150321261682243)

1 ]=> (define a (make_center_percent 10 0.05))
1 ]=> (define b (make_center_percent 7 0.1))
1 ]=> (par1 a b)
;Value 15: (4.108572478250646 . 4.126738580174241)
1 ]=> (par2 a b)
;Value 16: (4.11437691311514 . 4.1209167058546905)
1 ]=> (div_interval a b)
;Value 17: (1.4264307121449977 . 1.4307164307164308)
1 ]=> (div_interval a a)
;Value 18: (.9990004997501248 . 1.0010005002501252)
1 ]=> (center (div_interval a a))
;Value: 1.000000500000125


Lem is right.

|#



