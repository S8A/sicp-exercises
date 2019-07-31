(define (reverse l)
  (if (null? l) l (append (reverse (cdr l)) (list (car l)))))

