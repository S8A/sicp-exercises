(define (prodi term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (square x) (* x x))
(define (cube x) (* x x x))
(define (identity x) x)
(define (inc n) (+ n 1))

(define (factorial n)
  (prodi identity 1 inc n))


(define (pi_prod a b)
  (define (wallis n)
    (* (/ (* 2 n) (- (* 2 n) 1))
       (/ (* 2 n) (+ (* 2 n) 1))))
  (* 2.0 (prodi wallis a inc b)))


(define (prodr term a next b)
  (if (> a b)
      1
      (* (term a) (prodr term (next a) next b))))
