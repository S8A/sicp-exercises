(define (iterative_improve good? improve)
  (lambda (guess)
    (define (iter guess)
      (cond ((good? guess) guess)
            (else (iter (improve guess)))))
    (iter guess)))


(define (average x y) (/ (+ x y) 2))


(define (sqrt x)
  (define (good_enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative_improve good_enough? improve) 1.0))


(define tolerance 0.00001)
(define (fixed_point f first_guess)
  (define (close_enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative_improve close_enough? f) first_guess))
