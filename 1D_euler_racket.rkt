;;Libraries
(require 2htdp/image)
(require 2htdp/universe)

;;The ball in question
;; TOMATO the ball takes in a shape (circle)
;; Which is solid and tomato-colored
(define TOMATO (circle 10 "solid" "tomato"))

;; A ball takes in a y-position and a velocity
(define-struct ball (y v))

;; Initial ball, 50 repersents 10 pixels from the top for it to be
;; Initally visible
(define initial-point (make-ball 10 0))

;; CONSTANTS :D
(define WIDTH 150)
(define HEIGHT 700)
(define SCENE (empty-scene WIDTH HEIGHT))

;; Constants may be adjusted to maximize funsies :))
(define DRAG -0.47)
(define GRAVITY 125)
(define MASS 2.71)
(define STEPSIZE 0.1)
(define BOUNCE -0.9)

;; next-tick : ball -> ball
;; Takes in a ball and outputs a new ball
;; With an adjusted new postion and velocity
;; Using helper functions and Euler's Method :)
;; W Euler
(define (next-tick b)
  (bouncy
   (make-ball
    (get-position (ball-y b) (ball-v b))
    (get-velocity (ball-v b)
                  (get-acceleration
                   (get-drag (ball-v b)))))))


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
     (make-ball (sub1 HEIGHT) (* (ball-v b) BOUNCE))]
    [else b]))


;; draw-ball : ball -> img
;; Takes in the ball and places it
;; In the top center of the scene
(define (draw-ball b)
  (place-image TOMATO
               (/ WIDTH 2)
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

;; March 20th, 2026
;; Create to learn about Euler's Method
;; For a falling ball in 1D, also same idea
;; Being able to be done in 2D eventually ;)
;; Initially started in Google Sheets :D
