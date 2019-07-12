(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

#|
(expmod 2 13 7)
(rem (* 2 (expmod 2 12 7)) 7)
(rem (* 2 (rem (sq (expmod 2 6 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (expmod 2 3 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 (expmod 2 2 7)) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 (rem (sq (expmod 2 1 7)) 7)) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 (rem (sq (rem (* 2 (expmod 2 0 7)) 7)) 7)) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 (rem (sq (rem (* 2 1) 7)) 7)) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 (rem (sq 2) 7)) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 (rem 4 7)) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem (* 2 4) 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq (rem 8 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (sq 1) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem 1 7)) 7)) 7)
(rem (* 2 (rem (sq 1) 7)) 7)
(rem (* 2 (rem 1 7)) 7)
(rem (* 2 1) 7)
(rem 2 7)
2


(expmod 2 12 7)
(rem (sq (expmod 2 6 7)) 7)
(rem (sq (rem (sq (expmod 2 3 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 (expmod 2 2 7)) 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 (rem (sq (expmod 2 1 7)) 7)) 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 (rem (sq (rem (* 2 (expmod 2 0 7)) 7)) 7)) 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 (rem (sq (rem (* 2 1) 7)) 7)) 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 (rem (sq 2) 7)) 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 (rem 4 7)) 7)) 7)) 7)
(rem (sq (rem (sq (rem (* 2 4) 7)) 7)) 7)
(rem (sq (rem (sq (rem 8 7)) 7)) 7)
(rem (sq (rem (sq 1) 7)) 7)
(rem (sq (rem 1 7)) 7)
(rem (sq 1) 7)
(rem 1 7)
1


(expmod 2 6 7)
(rem (sq (expmod 2 3 7)) 7)
(rem (sq (rem (* 2 (expmod 2 2 7)) 7)) 7)
(rem (sq (rem (* 2 (rem (sq (expmod 2 1 7)) 7)) 7)) 7)
(rem (sq (rem (* 2 (rem (sq (rem (* 2 (expmod 2 0 7)) 7)) 7)) 7)) 7)
(rem (sq (rem (* 2 (rem (sq (rem (* 2 1) 7)) 7)) 7)) 7)
(rem (sq (rem (* 2 (rem (sq 2) 7)) 7)) 7)
(rem (sq (rem (* 2 (rem 4 7)) 7)) 7)
(rem (sq (rem (* 2 4) 7)) 7)
(rem (sq (rem 8 7)) 7)
(rem (sq 1) 7)
(rem 1 7)
1


(expmod 2 3 7)
(rem (* 2 (expmod 2 2 7)) 7)
(rem (* 2 (rem (sq (expmod 2 1 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (* 2 (expmod 2 0 7)) 7)) 7)) 7)
(rem (* 2 (rem (sq (rem (* 2 1) 7)) 7)) 7)
(rem (* 2 (rem (sq 2) 7)) 7)
(rem (* 2 (rem 4 7)) 7)
(rem (* 2 4) 7)
(rem 8 7)
1


(expmod 2 2 7)
(rem (sq (expmod 2 1 7)) 7)
(rem (sq (rem (* 2 (expmod 2 0 7)) 7)) 7)
(rem (sq (rem (* 2 1) 7)) 7)
(rem (sq 2) 7)
(rem 4 7)
4


(expmod 2 1 7)
(rem (* 2 (expmod 2 0 7)) 7)
(rem (* 2 1) 7)
2


(expmod 2 0 7)
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
