; From section 2.2.3.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

; From exercise 2.36.
(define (accumulate_n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate_n op init (map cdr seqs)))))


; Exercise 2.37.
(define (dot_product v w)
  (accumulate + 0 (map * v w)))
(define (matrix_p_vector m v)
  (map (lambda (row) (dot_product row v)) m))
(define (transpose mat)
  (accumulate_n cons (list) mat))
(define (matrix_p_matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix_p_vector cols row)) m)))


(define m1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 1 5 9)))
(define v1 (list 1 2 3 4))
(define v2 (list 5 6 7 8))
