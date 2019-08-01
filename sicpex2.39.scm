; From exercise 2.38.
(define (fold_right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold_right op initial (cdr sequence)))))


(define (fold_left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define test (list 1 2 3 4 5 6))


; Exercise 2.39.
(define (reverse1 sequence)
  (fold_right (lambda (x y) (append y (list x))) (list) sequence))
(define (reverse2 sequence)
  (fold_left (lambda (x y) (cons y x)) (list) sequence))

