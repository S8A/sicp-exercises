(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)   ; new origin
                     (make-vect 0.0 1.0)   ; new end of edge1
                     (make-vect 1.0 0.0))) ; new end of edge2

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 0.0 1.0))) ; new end of edge2

