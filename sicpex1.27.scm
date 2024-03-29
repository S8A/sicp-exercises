(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat_test n)
  (define (try_it a) (= (expmod a n n) a))
  (try_it (+ 1 (random (- n 1)))))

(define (carmichael_test n)
  (define (test a n)
    ;(display a)
    ;(newline)
    (cond ((= a n) #t)
          ((or (= a 0) (not (= (expmod a n n) a))) #f)
          (else (test (+ a 1) n))))
  (test 1 n))