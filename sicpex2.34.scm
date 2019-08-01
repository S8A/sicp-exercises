; From section 2.2.3.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

; Exercise 2.34.
(define (horner_eval x coefficient_sequence)
  (accumulate (lambda (this_coeff higher_terms)
                (+ this_coeff (* x higher_terms)))
              0
              coefficient_sequence))
