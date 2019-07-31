; Exercise 2.29.

(define (make_mobile left right)
  (list left right))

(define (make_branch length structure)
  (list length structure))


(define mobi (make_mobile (make_branch 50 20) (make_branch 30 10)))
(define equil (make_mobile (make_branch 50 20) (make_branch 40 25)))


; a
(define (left_branch mob) (car mob))
(define (right_branch mob) (car (cdr mob)))
(define (branch_length branch) (car branch))
(define (branch_structure branch) (car (cdr branch)))


; b
(define (branch_weight branch)
  (if (pair? (branch_structure branch))
      (total_weight (branch_structure branch))
      (branch_structure branch)))

(define (total_weight mob)
  (+ (branch_weight (left_branch mob)) (branch_weight (right_branch mob))))


; c
(define (branch_balanced? branch)
  (if (pair? (branch_structure branch))
      (balanced? (branch_structure branch))
      #t))

(define (balanced? mob)
  (define (torque branch) (* (branch_length branch) (branch_weight branch)))
  (let ((lb (left_branch mob))
        (rb (right_branch mob)))
    (and (= (torque lb) (torque rb))
         (branch_balanced? lb)
         (branch_balanced? rb))))


; d
#|(define (make_mobile left right)
  (cons left right))

(define (make_branch length structure)
  (cons length structure))

; Only had to change this:
(define (right_branch mob) (cdr mob))
(define (branch_structure branch) (cdr branch))


(define mobi (make_mobile (make_branch 50 20) (make_branch 30 10)))
(define equil (make_mobile (make_branch 50 20) (make_branch 40 25)))
|#
