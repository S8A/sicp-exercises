(define (p) (p))

(define (test x y) (if (= x 0) 0 y))

(test 0 (p))

#| Applicative order (substitution model)
(test 0 (p))
(if (= 0 0) 0 (p))
0 |#

#| Normal order
(test 0 (p))
(if (= 0 0) 0 (p))
(if (= 0 0) 0 (p))
...
infinitely |#