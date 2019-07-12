; From "Finding fixed points of functions" @ 1.3.3.
(define tolerance 0.00001)
(define (fixed_point f first_guess)
  (define (close_enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close_enough? guess next)
          next
          (try next))))
  (try first_guess))


; From 1.3.4.
(define (average a b) (/ (+ a b) 2.0))
(define (average_damp f)
  (lambda (x) (average x (f x))))


; From "Abstractions and first-class procedures" @ 1.3.4.
(define (fixed_point_of_transform g transform guess)
  (fixed_point (transform g) guess))


; From exercise 1.16.
(define (expti b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))


; From exercises 1.42. and 1.43.
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (- n 1)))))


; From exercise 1.44.
(define dx 0.00001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (nsmooth f n)
  ((repeated smooth n) f))


; General nth root procedure using r average dampings
(define (nroot n x r)
  (fixed_point_of_transform (lambda (y) (/ x (expti y (- n 1))))
                            (repeated average_damp r)
                            1.0))

; nth root procedure with automatic damping
(define (nth_root n x)
  (define r (floor (* 1.5 (log n))))
  (nroot n x r))
