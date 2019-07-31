#|

1 ]=> (define a (list 1 3 (list 5 7) 9))
;Value: a

1 ]=> a
;Value 15: (1 3 (5 7) 9)

1 ]=> (define b (list (list 7)))
;Value: b

1 ]=> b
;Value 16: ((7))

1 ]=> (define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;Value: c

1 ]=> c
;Value 17: (1 (2 (3 (4 (5 (6 7))))))

1 ]=> (car (cdr (car (cdr (cdr a)))))
;Value: 7

1 ]=> (car (car b))
;Value: 7

1 ]=> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
;Value: 7


|#