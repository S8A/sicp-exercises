; From section 2.2.1.

(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))


; Exercise 2.21
(define (square x) (* x x))

(define (square_list1 items)
  (if (null? items)
      items
      (cons (square (car items)) (square_list1 (cdr items)))))
(define (square_list2 items)
  (map square items))
