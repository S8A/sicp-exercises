(define (fold_right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold_right op initial (cdr sequence)))))


(define (fold_left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define test (list 1 2 3))


#| Requested values:
1 ]=> (fold_right / 1 test)
;Value: 3/2

1 ]=> (fold_left / 1 test)
;Value: 1/6

1 ]=> (fold_right list (list) test)
;Value 13: (1 (2 (3 ())))

1 ]=> (fold_left list (list) test)
;Value 14: (((() 1) 2) 3)


Explanation:
sequence: (1 2 3 4), initial: 0
fold_right: (op 1 (op 2 (op 3 (op 4 0))))
fold_left: (op (op (op (op (op 0 1) 2) 3) 4)

For fold_right and fold_left to give the same result, op must have the 
commutative property.
|#
