(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (square_check (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square_check x m)
  (if (and (not (= x 1)) (not (= x (- m 1))) (= (remainder (square x) m) 1))
    0
    (remainder (square x) m)))

(define (miller_rabin_test n)
  (define (try_it a) (= (expmod a (- n 1) n) 1))
  (try_it (+ 1 (random (- n 1)))))

(define (fast_prime? n times)
  (cond ((= times 0) true)
        ((miller_rabin_test n) (fast_prime? n (- times 1)))
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
