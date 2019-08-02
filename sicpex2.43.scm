(define nil (list))

; From section 2.2.1.
(define (list_ref items n)
  (if (= n 0)
      (car items)
      (list_ref (cdr items) (- n 1))))

(define (length items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
  (iter items 0))

; From exercise 2.33.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate_interval low high)
  (if (> low high)
      nil
      (cons low (enumerate_interval (+ low 1) high))))

; From section 2.2.3.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; From exercise 2.42.
(define (queens board_size)
  (define (queen_cols k)  
    (if (= k 0)
        (list empty_board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest_of_queens)
            (map (lambda (new_row)
                   (adjoin_position new_row k rest_of_queens))
                 (enumerate_interval 1 board_size)))
          (queen_cols (- k 1))))))
  (queen_cols board_size))


; Each pos is (x y), 1-index
(define (posit x y) (cons x y))
(define (col pos) (car pos))
(define (row pos) (cdr pos))

(define (same_col p1 p2) (= (col p1) (col p2)))
(define (same_row p1 p2) (= (row p1) (row p2)))
(define (same_diagonal_1 p1 p2)
  (= (- (col p1) (row p1)) (- (col p2) (row p2))))
(define (same_diagonal_2 p1 p2)
  (= (+ (col p1) (row p1)) (+ (col p2) (row p2))))

(define empty_board nil)

(define testz (list (posit 1 3) (posit 2 7) (posit 3 2) (posit 4 8) (posit 5 5) (posit 6 1) (posit 7 4) (posit 8 6)))

(define (safe? k positions)
  (define (attacks? p1 p2)
    (or (same_row p1 p2)
        (= (abs (- (row p1) (row p2))) (abs (- (col p1) (col p2))))))
  (define (iter p board)
    (if (null? board)
      #t
      (if (not (attacks? p (car board))) (iter p (cdr board)) #f)))
  (let ((kth (list_ref positions (- k 1)))
        (other_queens (filter (lambda (p) (not (= k (col p)))) positions)))
    (iter kth other_queens)
  ))

(define (adjoin_position row col positions)
  (append positions (list (posit col row))))


; Exercise 2.43.
(define (queenslow board_size)
  (define (queen_cols k)  
    (if (= k 0)
        (list empty_board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
           (lambda (new_row)
             (map (lambda (rest_of_queens)
                    (adjoin_position new_row k rest_of_queens))
                  (queen_cols (- k 1))))
           (enumerate_interval 1 board_size)))))
  (queen_cols board_size))

(define (timeq k type)
  (define (start_queens_test start_time)
    (if (= type 0)
      (length (queens k))
      (length (queenslow k)))
    (display (- (runtime) start_time)))
  (newline)
  (display "(")
  (display k)
  (display ", ")
  (start_queens_test (runtime))
  (display ")")
  (newline)
  #t)

(define (time_queens type)
  (define (iter k)
    (cond ((> k 8) (newline) #t)
          (else (timeq k type) (iter (+ k 1)))))
  (iter 1))


#|

It runs slower with that order because now the program has to calculate the 
solutions for each smaller version of the problem, not once, but once for each 
row. That

If the original version solved the puzzle in time T with k=8, Louis' version 
should solve it in time T^8, approximately.

Empirical evidence: https://www.desmos.com/calculator/yvnszwym3k
1 ]=> (time_queens 0)
(1, 0.)
(2, 0.)
(3, 0.)
(4, 0.)
(5, 0.)
(6, 1.9999999999999997e-2)
(7, .05)
(8, .27999999999999997)
;Value: #t

1 ]=> (time_queens 1)
(1, 0.)
(2, 0.)
(3, 0.)
(4, 1.0000000000000009e-2)
(5, .06)
(6, .94)
(7, 17.73)
(8, 387.26)
;Value: #t

|#
