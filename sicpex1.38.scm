(define (cont_frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


(define (approxe k)
  (define (d i)
    (if (= (remainder (- i 2) 3) 0)
        (/ (* 2 (+ i 1)) 3)
        1))
  (+ 2 (cont_frac (lambda (i) 1.0) d k)))
