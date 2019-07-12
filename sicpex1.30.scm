(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (cube x) (* x x x))

(define (inc n) (+ n 1))
(define (sum_cubes a b)
  (sum cube a inc b))


(define (identity x) x)

(define (sum_integers a b)
  (sum identity a inc b))


(define (pi_sum a b)
  (define (pi_term x) (/ 1.0 (* x (+ x 2))))
  (define (pi_next x) (+ x 4))
  (sum pi_term a pi_next b))


(define (integral f a b dx)
  (define (add_dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add_dx b) dx))


(define (even? n) (= (remainder n 2) 0))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk x)
    (define k (/ (- x a) h))
    (cond ((or (= k 0) (= k n)) (f x))
          ((even? k) (* 2 (f x)))
          (else (* 4 (f x)))))
  (define (nexta x) (+ x h))
  (if (even? n) (* (/ h 3.0) (sum yk a nexta b)) (simpson f a b (inc n))))

