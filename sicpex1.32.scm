(define (accumulatei combiner null_value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null_value))

(define (accumulater combiner null_value term a next b)
  (if (> a b)
      null_value
      (combiner (term a) (accumulater combiner null_value term (next a) next b))))

(define (sumi term a next b) (accumulatei + 0 term a next b))
(define (sumr term a next b) (accumulater + 0 term a next b))
(define (prodi term a next b) (accumulatei * 1 term a next b))
(define (prodr term a next b) (accumulater * 1 term a next b))



(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (identity x) x)
(define (inc n) (+ n 1))


(define (sum_cui a b) (sumi cube a inc b))
(define (sum_inti a b) (sumi identity a inc b))
(define (facti n) (prodi identity 1 inc n))
(define (sum_cur a b) (sumr cube a inc b))
(define (sum_intr a b) (sumr identity a inc b))
(define (factr n) (prodr identity 1 inc n))

(define (integral f a b dx)
  (define (add_dx x) (+ x dx))
  (* (sumi f (+ a (/ dx 2.0)) add_dx b) dx))


(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk x)
    (define k (/ (- x a) h))
    (cond ((or (= k 0) (= k n)) (f x))
          ((even? k) (* 2 (f x)))
          (else (* 4 (f x)))))
  (define (nexta x) (+ x h))
  (if (even? n) (* (/ h 3.0) (sumi yk a nexta b)) (simpson f a b (inc n))))


(define (pi_sum a b)
  (define (pi_term x) (/ 1.0 (* x (+ x 2))))
  (define (pi_next x) (+ x 4))
  (sumi pi_term a pi_next b))

(define (pi_prod a b)
  (define (wallis n)
    (* (/ (* 2 n) (- (* 2 n) 1))
       (/ (* 2 n) (+ (* 2 n) 1))))
  (* 2.0 (prodi wallis a inc b)))
