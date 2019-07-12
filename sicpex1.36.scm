(define tolerance 0.00001)
(define (fixed_point f first_guess)
  (define (close_enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (display " | ")
      (display next)
      (if (close_enough? guess next)
          next
          (try next))))
  (try first_guess))


(define (average a b) (/ (+ a b) 2.0))

(define xpowx
  (fixed_point (lambda (x) (/ (log 1000) (log x))) 2.0))
(define xpowxavg
  (fixed_point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
