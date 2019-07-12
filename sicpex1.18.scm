; Auxiliary functions
(define (even? n) (= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))


; Logarithmic iterative process
(define (* a b)
	(define (miter a b s)
		(cond ((= b 0) s)
			  ((even? b) (miter (double a) (halve b) s))
			  (else (miter a (- b 1) (+ s a)))))
	(miter a b 0))
