(define (for_each proc items)
  (cond ((null? items) #t)
        (else (proc (car items)) (for_each proc (cdr items)))))
