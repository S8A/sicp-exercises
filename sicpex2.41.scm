(define nil (list))

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


; Exercise 2.41.
(define (unique_triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate_interval 1 (- j 1))))
                      (enumerate_interval 1 (- i 1))))
           (enumerate_interval 1 n)))

(define (sum_to_amount triple k)
  (= k (+ (car triple) (car (cdr triple)) (cadr (cdr triple)))))

(define (triples_sum_k n k)
  (filter (lambda (x) (sum_to_amount x k)) (unique_triples n)))


