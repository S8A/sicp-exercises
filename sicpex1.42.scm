(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

