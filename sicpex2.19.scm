(define (count_change amount)
  (cc amount us_coins))
(define (cc amount coin_values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no_more? coin_values)) 0)
        (else
         (+ (cc amount
                (except_first_denomination coin_values))
            (cc (- amount
                   (first_denomination coin_values))
                coin_values)))))

(define us_coins (list 25 10 5 1 50))
(define uk_coins (list 100 50 20 10 5 2 1 0.5))

(define (first_denomination coins) (car coins))
(define (no_more? coins) (null? coins))
(define (except_first_denomination coins) (cdr coins))
