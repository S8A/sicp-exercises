; From section 2.2.1.
(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))

; From section 2.2.2.
(define (count_leaves1 x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count_leaves1 (car x)) (count_leaves1 (cdr x))))))

; From section 2.2.3.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

; From exercise 2.33.
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


; Exercise 2.35.
(define tree (list (list 1 2) (list 3 4) 5 6))

(define (count_leaves t)
  (accumulate + 0 (map (lambda (x) (if (not (pair? x)) 1 (length x))) t)))
