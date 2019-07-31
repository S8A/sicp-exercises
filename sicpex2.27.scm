; From exercise 2.18.
(define (reverse l)
  (if (null? l) l (append (reverse (cdr l)) (list (car l)))))


; Exercise 2.27.
(define (deep_reverse l)
  (cond ((null? l) (list))
        ((not (list? l)) l)
        (else (append (deep_reverse (cdr l)) (list (deep_reverse (car l)))))))

(define x (list (list 1 2) (list 3 4)))
