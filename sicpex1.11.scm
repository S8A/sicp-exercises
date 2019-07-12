; Recursive
(define (fr n)
	(if (< n 3)
		n
		(+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3))))))

#| 
(fr 5)
(+ (fr 4) (* 2 (fr 3)) (* 3 (fr 2)))
(+ (+ (fr 3) (* 2 (fr 2)) (* 3 (fr 1))) (* 2 (+ (fr 2) (* 2 (fr 1)) (* 3 (fr 0)))) (* 3 2))
(+ (+ (+ (fr 2) (* 2 (fr 1)) (* 3 (fr 0))) (* 2 2) (* 3 1)) (* 2 (+ 2 (* 2 1) (* 3 0))) 6)
(+ (+ (+ 2 (* 2 1) (* 3 0)) 4 3) (* 2 (+ 2 2 0)) 6)
(+ (+ (+ 2 2 0) 4 3) (* 2 4) 6)
(+ (+ 4 4 3) 8 6)
(+ 11 8 6)
25
 |#


; Iterative
(define (fi n)
	(define (fiter a b c k)
		(if (< k 3)
			c
			(fiter b c (+ c (* 2 b) (* 3 a)) (- k 1))))
	(if (< n 3) n (fiter 0 1 2 n)))

#| 
(fr 5)
(fiter 0 1 2 5)
(fiter 1 2 4 4)
(fiter 2 4 11 3)
(fiter 4 11 25 2)
25
 |#
