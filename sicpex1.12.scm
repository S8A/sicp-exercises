(define (pascal n pos)
	(if (or (= pos 1) (= pos n))
		1
		(+ (pascal (- n 1) (- pos 1)) (pascal (- n 1) pos))))