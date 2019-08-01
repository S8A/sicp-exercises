(define nil (list))

; From exercise 1.23.
(define (square x) (* x x))

(define (smallest_divisor n)
  (define (next n) (if (= n 2) 3 (+ n 2)))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find_divisor n test_divisor)
    (cond ((> (square test_divisor) n) n)
          ((divides? test_divisor n) test_divisor)
          (else (find_divisor n (next test_divisor)))))
  (find_divisor n 2))

(define (prime? n) (= n (smallest_divisor n)))

; From exercise 2.33.
(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate_interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate_interval (+ low 1) high))))

; From section 2.2.3.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime_sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make_pair_sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime_sum_pairs1 n)
  (map make_pair_sum
       (filter prime_sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate_interval 1 (- i 1))))
                (enumerate_interval 1 n)))))


; Exercise 2.40.
(define (unique_pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate_interval 1 (- i 1))))
           (enumerate_interval 1 n)))

(define (prime_sum_pairs n)
  (map make_pair_sum (filter prime_sum? (unique_pairs n))))

