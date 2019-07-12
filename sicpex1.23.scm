(define (square x) (* x x))

(define (smallest_divisor n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (find_divisor n test_divisor)
      (cond ((> (square test_divisor) n) n)
            ((divides? test_divisor n) test_divisor)
            (else (find_divisor n (next test_divisor)))))
    (find_divisor n 2))


(define (prime? n) (= n (smallest_divisor n)))
(define (next n) (if (= n 2) 3 (+ n 2)))


(define (timed_prime_test n)
  (newline)
  (display n)
  (start_prime_test n (runtime)))
(define (start_prime_test n start_time)
  (if (prime? n)
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
