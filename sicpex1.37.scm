; Iterative
(define (cont_frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (phi k)
  (/ 1.0 (cont_frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k)))


(define (cfrac n d k)
  (define (cfracr i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cfracr (+ i 1))))))
  (cfracr 1))

(define (phir k)
  (/ 1.0 (cfrac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))
