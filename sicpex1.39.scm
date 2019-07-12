(define (cont_frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


(define (square x) (* x x))

(define (tan_cf x k)
  (cont_frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

; pi/6 =0.5235987755982989
; tan(pi/6) = 0.5773502691896258
