(define (fast_expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast_expt b (/ n 2))))
          (else (* b (fast_expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast_expt base exp) m))

#|
(expmod 2 13 7)
(rem (fexp 2 13) 7)
(rem (* 2 (fexp 2 12)) 7)
(rem (* 2 (sq (fexp 2 6))) 7)
(rem (* 2 (sq (sq (fexp 2 3)))) 7)
(rem (* 2 (sq (sq (* 2 (fexp 2 2))))) 7)
(rem (* 2 (sq (sq (* 2 (sq (fexp 2 1)))))) 7)
(rem (* 2 (sq (sq (* 2 (sq (* 2 (fexp 2 0))))))) 7)
(rem (* 2 (sq (sq (* 2 (sq (* 2 1)))))) 7)
(rem (* 2 (sq (sq (* 2 (sq 4))))) 7)
(rem (* 2 (sq (sq (* 2 16)))) 7)
(rem (* 2 (sq (sq 32))) 7)
(rem (* 2 (sq 1024)) 7)
(rem 2097152 7)
1
|#

(define (fermat_test n)
  (define (try_it a) (= (expmod a n n) a))
  (try_it (+ 1 (random (- n 1)))))

(define (fast_prime? n times)
  (cond ((= times 0) true)
        ((fermat_test n) (fast_prime? n (- times 1)))
        (else false)))


(define (timed_prime_test n)
  (newline)
  (display n)
  (start_prime_test n (runtime)))
(define (start_prime_test n start_time)
  (if (fast_prime? n 1000)
      (report_prime (- (runtime) start_time)) #f))
(define (report_prime elapsed_time)
  (display " *** ")
  (display elapsed_time))


(define (search_for_primes min max counter)
  (cond ((= counter 0) 0)
        ((> min max) 0)
        ((even? min) (search_for_primes (+ min 1) max counter))
        ((timed_prime_test min) (search_for_primes (+ min 2) max (- counter 1)))
        (else (search_for_primes (+ min 2) max counter))))

(define (sfp n) (search_for_primes n (* n 10) 3))