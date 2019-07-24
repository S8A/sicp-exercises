(define zero (lambda (f) (lambda (x) x)))

(define (add_1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

#|
(add_1 zero)
(add_1 (lambda (f) (lambda (x) x)))
((lambda (f) (lambda (x) (f ((n f) x)))) (lambda (f) (lambda (x) x)))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))
wtf?

oh, thanks Wikipedia: it represents applying f to x one time.
|#


(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

#|
; Better with repeated composition as implented in exercise 1.43.
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (- n 1)))))

(define onecf (lambda (f) (repeated f 1)))
(define twocf (lambda (f) (repeated f 2)))

; Damn, shouldn't use numbers
|#

; The addition of two Church numerals a and b is applying the function a + b 
; times. According to Wikipedia (and thinking a bit), applying the function 
; a + b times is the same as applying the function a times to the result of 
; applying the function b times.

(define (plus a b)
  (lambda (f) (lambda (x) (f ((a (b f)) x)))))

#|
(plus one zero)
(lambda (f) (lambda (x) (f ((one (zero f)) x))))
(lambda (f) (lambda (x) (f ((one (lambda (x) x)) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) (lambda (x) x)) x))))
(lambda (f) (lambda (x) (f ((lambda (x) ((lambda (x) x) x)) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

(plus one two)
(lambda (f) (lambda (x) (f ((one ((lambda (f) (lambda (x) (f (f x)))) f)) x))))
(lambda (f) (lambda (x) (f ((one (lambda (x) (f (f x)))) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) (lambda (x) (f (f x)))) x))))
(lambda (f) (lambda (x) (f ((lambda (x) ((lambda (x) (f (f x))) x)) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f (f x))) x))))
(lambda (f) (lambda (x) (f (f (f x)))))

Perfect
|#
