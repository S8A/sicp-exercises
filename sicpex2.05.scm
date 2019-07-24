; From exercise 1.16.
(define (power b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

; Exercise 2.5.
(define (cons a b) (* (power 2 a) (power 3 b)))

(define (car p)
  (define (iter n a)
    (cond ((= n 1) a)
          ((= (remainder n 3) 0) (iter (/ n 3) a))
          (else (iter (/ n 2) (+ a 1)))))
  (iter p 0))

(define (cdr p)
  (define (iter n b)
    (cond ((= n 1) b)
          ((= (remainder n 2) 0) (iter (/ n 2) b))
          (else (iter (/ n 3) (+ b 1)))))
  (iter p 0))
