; From section 2.2.3.
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (even? n)))
(define (square n) (* n n))

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

(define (enumerate_tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate_tree (car tree))
                      (enumerate_tree (cdr tree))))))


; Exercise 2.33.
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

