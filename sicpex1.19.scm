; Auxiliary functions
(define (square x) (* x x))

; Optimised Fibonacci sequence
(define (fib n)
  (define (fib_iter a b p q k)
    (cond ((= k 0) b)
          ((even? k) (fib_iter a
                               b
                               (+ (square q) (square p))    ; compute p'
                               (+ (square q) (* 2 p q))     ; compute q'
                               (/ k 2)))
          (else (fib_iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- k 1)))))
  (fib_iter 1 0 0 1 n))
