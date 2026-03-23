(define-struct world (earth moon))

(define-struct body (x y vx vy mass))

(define EARTH (make-body 1 1 1 1 1))
(define MOON (make-body 2 2 2 2 2))

(define GRAVITY 100)
(define STEPSIZE 0.1)

(define (distance w)
  (sqrt
   (+
    (sqr (- (body-x (world-earth w)) (body-x (world-moon w))))
    (sqr (- (body-y (world-earth w)) (body-y (world-moon w)))))))

(define (force m1 m2 dist)
  (/ (* GRAVITY m1 m2) (sqr dist)))

;;March 23rd, 2026
