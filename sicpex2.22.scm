; Louis Reasoner's failed attempts

(define (square_list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items (list)))

(define (square_list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))


; Exercise 2.22.

(define (square_list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items (list)))

#|
Louis' first procedure is obviously backwards, since in every iteration it
creates a pair whose first element is the next square and the second is the 
list of previous squares.

His second procedure appends the squares in the right order but in the wrong 
way. In each iteration, it creates a pair whose first element is the list of 
previous squares and the second element is the next square. Lists are pairs 
nested on the second element, he's nesting on the first.

The solution is to append the next square as a list to the list of previous 
squares.

1 ]=> (define l (list 1 2 3 4 5 6 7 8 9 10))
;Value: l
1 ]=> (square_list1 l)
;Value 13: (100 81 64 49 36 25 16 9 4 1)
1 ]=> (square_list2 l)
;Value 14: ((((((((((() . 1) . 4) . 9) . 16) . 25) . 36) . 49) . 64) . 81) . 100)
1 ]=> (square_list3 l)
;Value 15: (1 4 9 16 25 36 49 64 81 100)
|#
