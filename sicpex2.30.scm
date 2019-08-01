; From section 2.2.1.
(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale_tree1 tree factor)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale_tree1 (car tree) factor)
                    (scale_tree1 (cdr tree) factor)))))

(define (scale_tree2 tree factor)
  (map (lambda (sub_tree)
         (if (pair? sub_tree)
             (scale_tree2 sub_tree factor)
             (* sub_tree factor)))
       tree))


; Exercise 2.30.
(define (square x) (* x x))

(define (square_tree1 tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (square_tree1 (car tree))
                    (square_tree1 (cdr tree))))))

(define (square_tree2 tree)
  (map (lambda (sub_tree)
         (if (pair? sub_tree)
             (square_tree2 sub_tree)
             (square sub_tree)))
       tree))


(define test (list 1 (list 2 (list 3 4) 5) (list 6 7)))
