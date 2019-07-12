; Filtered accumulate procedure
(define (filtered_accumulate combiner null_value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a) (combiner result (term a)) result))))
  (iter a null_value))


; Auxiliary procedures
(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (identity x) x)

(define (smallest_divisor n)
  (define (next n) (if (= n 2) 3 (+ n 2)))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find_divisor n test_divisor)
    (cond ((> (square test_divisor) n) n)
          ((divides? test_divisor n) test_divisor)
          (else (find_divisor n (next test_divisor)))))
  (find_divisor n 2))

(define (prime? n) (= n (smallest_divisor n)))

(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))


; Sum of the squares of prime numbers in the interval a to b
(define (sum_sqprimes a b)
  (filtered_accumulate + 0 square a inc b prime?))


; Product of positive integers less than n relatively prime to n
(define (prod_nprimes n)
  (define (relatively_prime i) (= (gcd i n) 1))
  (filtered_accumulate * 1 identity 1 inc (- n 1) relatively_prime))
