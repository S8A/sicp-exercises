; Auxiliary functions
(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))


; Linear recursive process (book)
(define (expt_lir b n)
    (if (= n 0) 1 (* b (expt_lr b (- n 1)))))


; Linear iterative process (book)
(define (expt_lii b n)
    (define (expt_iter b counter product)
        (if (= counter 0)
            product
            (expt_iter b (- counter 1) (* b product))))
    (expt_iter b n 1))


; Logarithmic recursive process (book)
(define (fast_expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast_expt b (/ n 2))))
          (else (* b (fast_expt b (- n 1))))))


; Logarithmic iterative process
(define (expt_loi b n)
    (define (expt_iter b n a)
        (cond ((= n 0) a)
              ((even? n) (expt_iter (square b) (/ n 2) a))
              (else (expt_iter b (- n 1) (* a b)))))
    (expt_iter b n 1))

#| Transformations :.
Odd:    b <- b      n <- n - 1    a <- a * b
Even:   b <- b^2    n <- n / 2    a <- a
|#

