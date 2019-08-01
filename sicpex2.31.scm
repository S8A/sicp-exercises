; From section 2.2.1.
(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))


; Exercise 2.31.
(define (tree_map proc tree)
  (map (lambda (sub_tree)
         (if (pair? sub_tree)
             (tree_map proc sub_tree)
             (proc sub_tree)))
       tree))

(define (scale_tree tree factor)
  (tree_map (lambda (x) (* factor x)) tree))

(define (square x) (* x x))
(define (square_tree tree) (tree_map square tree))

(define test (list 1 (list 2 (list 3 4) 5) (list 6 7)))

