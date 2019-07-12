; Auxiliary functions
(define (even? n) (= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))


#|; Linear recursive multiplication (book)
(define (* a b)
	(if (= b 0)
    	0
    	(+ a (* a (- b 1)))))
|#

; Logarithmic recursive process
(define (* a b)
	(cond ((or (= a 0) (= b 0)) 0)
		  ((even? b) (* (double a) (halve b)))
		  (else (+ a (* a (- b 1))))))