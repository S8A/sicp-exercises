(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

#| Verification
(define z (cons 1 2))
(lambda (m) (m x y))

(car z)
(z (lambda (p q) p))
((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p) 1 2)
1
|#


(define (cdr z)
  (z (lambda (p q) q)))
