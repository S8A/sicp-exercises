(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

#|
The subsets of s are the subsets of s without its first item plus the subsets 
of s with its first item.

Explanation in informal syntax
s = (1 2 3)
cdr = (2 3)
(subsets (1 2 3)) = (append (subsets (2 3)) (1, subsets (2 3)))
s = (2 3)
cdr = (3)
(subsets (2 3)) = (append (subsets (3)) (2, subsets (3)))
s = (3)
cdr = ()
(subsets (3)) = (append (subsets ()) (3, subsets ()))
s = ()
subsets () = ()
(subsets (3)) = (append () (3)) = (() (3))
(subsets (2 3)) = (append (() (3)) ((2) (2 3))) = (() (3) (2) (2 3))
(subsets (1 2 3)) = (append (() (3) (2) (2 3)) ((1) (1 3) (1 2) (1 2 3)))
                  = (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
= ()
|#
