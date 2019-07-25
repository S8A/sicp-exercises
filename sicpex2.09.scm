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


(define (width x)
  (/ (abs (- (upper_bound x) (lower_bound x))) 2))


#|
Given numbers x±a and y±b, their intervals are (x-a, x+a) and (y-b, y+b)

In addition, the width is a function only of the widths of each interval.
width(x) = (x+a-(x-a))/2 = (x+a-x+a)/2 = 2a/2 = a
width(y) = (y+b-(y-b))/2 = (y+b-y+b)/2 = 2b/2 = b
x + y
(x-a, x+a) + (y-b, y+b)
(x-a+y-b, x+a+y+b)
(x+y-a-b, x+y+a+b)
width(x+y) =
((x+y+a+b) - (x+y-a-b))/2
(x+y+a+b-x-y+a+b)/2
(a+b+a+b)/2
(2a+2b)/2
a+b
width(x) + width(y)
For substraction it's the same, since it's just addition with the negative of 
the second interval.

In multiplication, things are not so straightforward. The width depends on
the signs and magnitudes of each component.
x * y
(x-a, x+a) * (y-b, y+b)
(min(p1,p2,p3,p4), max(p1,p2,p3,p4)) where
p1 = (x-a)(y-b) = xy - bx - ay + ab
p2 = (x-a)(y+b) = xy + bx - ay - ab
p3 = (x+a)(y-b) = xy - bx + ay - ab
p4 = (x+a)(y+b) = xy + bx + ay + ab
For example, checking if p1 < p2 is not conclusive:
xy - bx - ay + ab < xy + bx - ay - ab
- bx + ab < bx - ab
2ab < 2bx
a < x knowing that b > 0
But a may be greater than or equal to x. So the product itself cannot be 
expressed as a function of the widths of x and y. Therefore, the width of 
the product can't be either. And because the division of two intervals 
is just the product of the first and the inverse of the second, it can't 
be either.

To understand all of this better, let's use two examples.
x = (-3,5) => width(x) = 4
y = (2,8) => width(y) = 3

x+y = (-3+2,5+8) = (-1,13)
=> width(x+y) = 14/2 = 7 = width(x) + width(y)

x*y = (min(-6,-24,10,40), max(-6,-24,10,40)) = (-24,40)
=> width(x*y) = 64/2 = 32
32 is coincidentally 4^3/2, but that doesn't matter since it's not always 
this way.

x = (-4,6) => width(x) = 5
y = (2,8) => width(y) = 3

x+y = (-4+2,6+8) = (-2,14)
=> width(x+y) = 16/2 = 8 = width(x) + width(y)

x*y = (min(-8,-32,12,48), max(-8,-32,12,48)) = (-32,48)
=> width(x*y) = 80/2 = 40
40 != 5^3/2 => 40 != 62.5
That's how it can be disproved that the width of the product of two intervals 
is a function only of the width of each interval.
|#
