(define x (list 1 2 3))
(define y (list 4 5 6))


(define a (append x y))
(define b (cons x y))
(define c (list x y))

#|
1 ]=> a
;Value 13: (1 2 3 4 5 6)

1 ]=> b
;Value 14: ((1 2 3) 4 5 6)

1 ]=> c
;Value 15: ((1 2 3) (4 5 6))

|#
