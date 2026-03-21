;;Libraries
(require 2htdp/image)
(require 2htdp/universe)

;;The ball in question
;; TOMATO the ball takes in a shape (circle)
;; Which is solid and tomato-colored
(define TOMATO (circle 10 "solid" "tomato"))

;; A ball takes in a x & y position and x & y velocity
(define-struct ball (x y vx vy))

;; Initial ball
(define initial-point (make-ball 0 0 500 -500))

;; CONSTANTS :D
(define WIDTH 1500)
(define HEIGHT 700)
(define SCENE (empty-scene WIDTH HEIGHT))

;; Constants may be adjusted to maximize funsies :))
(define DRAG -0.3)
(define GRAVITY 100)
(define MASS 1.61)
(define STEPSIZE 0.1)
(define BOUNCE -.95)

;; next-tick : ball -> ball
;; Takes in a ball and outputs a new ball
;; With an adjusted new postion and velocity
;; Using helper functions and Euler's Method :)
;; W Euler
(define (next-tick b)
  (bouncy
   (make-ball
    (get-position (ball-x b) (ball-vx b))
    (get-position (ball-y b) (ball-vy b))
    (get-velocity (ball-vx b)
                  (/ (get-drag (ball-vx b)) MASS))
    (get-velocity (ball-vy b)
                  (get-acceleration
                   (get-drag (ball-vy b)))))))


;; get-drag : num -> num
;; Adjusts velocity to account for drag resistance
;; Equation: Drag * Velocity
(define (get-drag velocity)
  (* DRAG velocity))

;; get-accleration : num -> num
;; Takes in air-resistance and outputs
;; Appropriate acceleration accounting drag
;; Equation: (Gravity * Drag)/Mass
(define (get-acceleration drag)
  (/ (+ GRAVITY drag) MASS))

;; get-velocity : num num -> num
;; Takes in the old velocity and accleration
;; And uses acceleration w/ stepsize to get
;; The new velocity of the ball
;; Equation: Old Velocity + (Acceleration * Stepsize) = New Velocity
(define (get-velocity velocity acceleration)
  (+ velocity (* acceleration STEPSIZE)))

;; get-position : num num -> num
;; Takes in the old position and velocity
;; And changes positon based off of velocity
;; Equation: Old Position + (Old Velocity * Stepsize) = New Position
(define (get-position position velocity)
  (+ position (* velocity STEPSIZE)))

;; bouncy : ball -> ball
;; Takes in a ball and outputs a new ball if it has it the ground
;; Going in the opposite direction
;; One pixel off the ground ;)
(define (bouncy b)
  (cond
    [(>= (ball-y b) HEIGHT)
     (make-ball (ball-x b) (sub1 HEIGHT) (ball-vx b) (* (ball-vy b) BOUNCE))]
    [(<= (ball-y b) 0)
     (make-ball (ball-x b) 1 (ball-vx b) (* (ball-vy b) BOUNCE))]
    [(>= (ball-x b) WIDTH)
     (make-ball (sub1 WIDTH) (ball-y b) (* (ball-vx b) BOUNCE) (ball-vy b))]
    [(<= (ball-x b) 0)
     (make-ball 1 (ball-y b) (* (ball-vx b) BOUNCE) (ball-vy b))]
    [else b]))


;; draw-ball : ball -> img
;; Takes in the ball and places it
;; In the top center of the scene
(define (draw-ball b)
  (place-image TOMATO
               (ball-x b)
               (ball-y b)
               SCENE))

;; euler-ball : ball -> ball
;; Takes in a ball and runs the simulation
(define (euler-ball initial-ball)
  (big-bang initial-ball
    [on-tick next-tick]
    [to-draw draw-ball]))


;;This is how you animate the ball :D
(euler-ball initial-point)

;; March 21th, 2026
;; Create to learn about Euler's Method
;; The bouncing ball has been recreated from a 2D
;; Ball Googlespread sheet
