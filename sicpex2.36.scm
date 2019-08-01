; From section 2.2.3.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

; Exercise 2.36.
(define (accumulate_n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate_n op init (map cdr seqs)))))

(define seqsy (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
