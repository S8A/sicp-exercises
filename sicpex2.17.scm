; From section 2.2.1

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


; Exercise 2.17

(define (last_pair l)
  (if (null? l)
      l
      (if (= (length l) 2) l (last_pair (cdr l)))))

