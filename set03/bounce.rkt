;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(require "extras.rkt")

(require rackunit)

(require 2htdp/image)

(require 2htdp/universe)

(define TIME-ON-TASK 10) ; hours

;(check-location "03" "bounce.rkt")



(provide INITIAL-WORLD)

(provide next-world)

(provide key-handler)

(provide mouse-handler)

(provide world-ball)

(provide world-paused?)

(provide ticks-since-click)

(provide score)

(provide ball-x)

(provide ball-y)




; graphicial constants

(define WIDTH 300)  ; Pixels

(define HEIGHT 400) ; Pixels

(define SCENE (empty-scene WIDTH HEIGHT)) ; Image

(define RADIUS 20)  ; Pixels

(define RIGHT-WALL (- WIDTH RADIUS)) ; Coordinate

(define LEFT-WALL RADIUS) ; Coordinate

(define UP-WALL RADIUS)   ; Coordinate

(define DOWN-WALL (- HEIGHT RADIUS)) ;Coordinate

(define BALL-IMG (circle RADIUS "solid" "black")) ; Image

(define X-VELOCITY 3)   ; Absolute Velocity in X direction, Pixels/tick
 
(define INNER-EXPAND 2) ; Pixels/tick

(define OUTER-EXPAND 4) ; Pixels/tick

(define ACCELERATION 1) ; Pixels/tick^2

(define BC 0.9)

(define PAUSE (text "*PAUSED*" 15 "black")) ; Image

(define COUNT (text "count: " 15 "black"))  ; Image

(define ε 0.001)





; data definition __________________________________


; A Velocity is a Real
; INTERP: represents the current phycial velocity of the ball;
;         Positive means the ball is now moving in X/Y Coordinate's
;         positive direction; Negtive means toward oppsite direction.
(define vec1 20)   ; ball's speed is 20 in positive X/Y direction
(define vec2 0)    ; ball's speed is 0 in X/Y direction
(define vec3 -10)  ; ball's speed is 10 in negtive X/Y direction


; A PropertyX is a (make-propertyX Coordinate Velocity)
; INTERP: Coordinate represents X coordinate of the ball, Velocity represents
;         speed in X coordinate direction of the ball, which is always 3 or -3
(define-struct propertyX[pos v])     
(define px1 (make-propertyX 150 3))   ; X Coordinate 150, speed 3
(define px2 (make-propertyX 20 3))    ; X Coordinate 20, speed 3
(define px3 (make-propertyX 280 -3))   ; X Coordinate 280, speed -3
; TEMPLATE:
; propertyX-fn : PropertyX -> ???
(define (propertyX-fn px)
  (... (propertyX-pos py) ... (propertyX-v py) ...))


; A PropertyY is a (make-propertyY(Coordinate, Velocity))
; INTERP: Coordinate represents Y coordinate of the ball;
;         Velocity represents speed in Y coordinate direction of the ball.
(define-struct propertyY[pos v])
(define py1 (make-propertyY 150 10))  ; Y Coordinate 150, speed 10
(define py2 (make-propertyY 20 0))    ; Y Coordinate 20, speed 0
(define py3 (make-propertyY 280 -13)) ; Y Coordinate 280, speed -13
; TEMPLATE:
; propertyY-fn : PropertyY -> ???
(define (propertyY-fn py)
  (... (propertyY-pos py) ... (propertyX-v py) ...))


; A Score is a NonNegInt
; INTERP:  the score is a count of the number of times the ball was
;          clicked since the last time the ball hit the ground
(define score1 0) ; ball was not clicked sine the last time the ball hit the ground
(define score2 1) ; ball was cloicked once since last time the ball hit the ground


; A Explosion is one of:
; - [0, 10)
; INTERP: the number of ticks since the last explosion, if there's
;         currently one. 0 means no explosion.
(define explostion1 0) ; no explostion
(define explostion2 9) ; 9 ticks since the last explosion
(define explostion3 5) ; 5 ticks since the last explosion


; A Ball is a (make-ball propertyX propertyY)
; INTERP: propertyX represents the ball's phycial properties in the horizontal
;         direction; propertyY for vertical direction.
(define-struct ball[propertyX propertyY])
(define b1 (make-ball (make-propertyX 150 3)    ; ball's position(150, 20)
                      (make-propertyY 20 4)))   ; ball's velocity(3, 4)
(define b2 (make-ball (make-propertyX 280 -3)   ; ball's position(280, 380)
                      (make-propertyY 380 -1))) ; ball's velocity(-3, -1)
(define b3 (make-ball (make-propertyX 20 3)     ; ball's position(20, 380) 
                      (make-propertyY 380 0)))  ; ball's velocity(3, 0)
; TEMPLATE:
; ball-fn : Ball -> ???
(define (ball-fn b)
  (... (ball-propertyX b) ... (ball-propertyY b) ...))


; A World is a (make-world(Ball, Boolean, Score, Explosion))
; INTERP: Ball represents the moving ball; Boolean means whether the current world
;         is paused; Score represents the clicks since last time hit ground; Explosion
;         represents the ticks since last explosion, 0 means no explostion.
(define-struct world[cball paused score explosion])
(define INITIAL-WORLD (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 0))
                                  #false
                                  0
                                  0))
; TEMPLATE:
; world-fn : World -> ???
(define (world-fn w)
  (... (world-cball w) ... (world-paused w) ...
       (world-score w) ... (world-explosion w) ...))








; World Functions:____________________________


; run : World -> Image
; Simulates a Ball Bouncing in the ground within a retricted area.
; The Ball moves with a constants speed 3 in the X direction, with
; acceleration toward positive Y direction, with bounce coefficient 0.9
; STRATEGY: function composition
(define (run w)
  (big-bang w
            (on-tick next-world)
            (to-draw render-world)
            (on-key key-handler)
            (on-mouse mouse-handler)))


; next-world : World -> World
; Computes the next World state from the given World state.
(begin-for-test 
  (check-equal? (next-world INITIAL-WORLD)
                (make-world (make-ball (make-propertyX 153 X-VELOCITY) 
                                             (make-propertyY (round/ε 20.5) 1))
                                  #false
                                  0
                                  0)
                "next world after one tick of the INITIAL-WORLD")
  (check-equal? (next-world (make-world (make-ball (make-propertyX 153 X-VELOCITY) 
                                             (make-propertyY (round/ε 20.5) 1))
                                  #true
                                  0
                                  0))
                (make-world (make-ball (make-propertyX 153 X-VELOCITY) 
                                             (make-propertyY (round/ε 20.5) 1))
                                  #true
                                  0
                                  0)
                "next world remains the same if current world is paused")
  (check-equal? (next-world (make-world (make-ball (make-propertyX 153 X-VELOCITY) 
                                             (make-propertyY 380 9))
                                  #false
                                  3
                                  2))
                (make-world (make-ball (make-propertyX 156 X-VELOCITY) 
                                             (make-propertyY 380 (round/ε -8.1)))
                                  #false
                                  0
                                  0)
                "both score and explosion effects resetted after the hit the ground"))
; STRATEGY: data decomposition on w : World
(define (next-world w)
  (cond
    [(world-paused? w) w]
    [else (make-world (next-ball (world-ball w))
                  (world-paused w)
                  (if (hit-ground-next-tick? (ball-propertyY (world-ball w)))
                      0
                      (world-score w))
                  (if (hit-ground-next-tick? (ball-propertyY (world-ball w)))
                      0
                      (next-explosion (world-explosion w))))]))






; render-world : World -> Image
; Presents the current world state into Image
(begin-for-test
  (check-equal? (render-world INITIAL-WORLD)
                    (count-ball-img INITIAL-WORLD)
                    "render the INITIAL-WORLD")
  (check-equal? (render-world (make-world (make-ball (make-propertyX 156 X-VELOCITY) 
                                             (make-propertyY 380 (round/ε -8.1)))
                                  #true
                                  0
                                  0))
                    (pause-count-ball-img (make-world (make-ball (make-propertyX 156 X-VELOCITY) 
                                             (make-propertyY 380 (round/ε -8.1)))
                                  #true
                                  0
                                  0))
                    "render the paused world"))
; STRATEGY: function composition
(define (render-world w)
  (if (world-paused? w)
      (pause-count-ball-img w)
      (count-ball-img w)))


; key-handler : World KeyEvent -> World
; Computes the next world after a key pressed.
(begin-for-test
  (check-equal? (key-handler INITIAL-WORLD "p")
              (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 0))
                                  #true
                                  0
                                  0)
              "p pressed, the world will paused")
  (check-equal? (key-handler INITIAL-WORLD " ")
              (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 0))
                                  #false
                                  0
                                  0)
              "space pressed, nothing happens")
  (check-equal? (key-handler (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 0))
                                  #true
                                  0
                                  0)
                             "p")
              (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 0))
                                  #false
                                  0
                                  0)
              "p pressed, the world will continue."))
; STRATEGY: data decomposition on ke: KeyEvent
(define (key-handler w ke)
  (cond
    [(pause? ke) (if (world-paused? w)
                     (make-world (world-ball w) #false (world-score w) (world-explosion w))
                     (make-world (world-ball w) #true (world-score w) (world-explosion w)))]
    [else w]))



; mouse-handler : World Integer Integer MouseEvent -> World
; Computes the next world after a mouse event.
(begin-for-test
  (check-equal? (mouse-handler INITIAL-WORLD 0 0 "button-up")
                INITIAL-WORLD
                "button released, nothing happens")
  (check-equal? (mouse-handler (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 10))
                                  #false
                                  0
                                  0) 
                               150 20 "button-down")
                (set-explosion (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 10))
                                  #false
                                  0
                                  0))
                "mouse on the ball, and button pressed, the world will explode, the ball heading down")
  (check-equal? (mouse-handler (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 -10))
                                  #false
                                  0
                                  0)
                               150 20 "button-down")
                (set-explosion (make-world (make-ball (make-propertyX 150 X-VELOCITY) 
                                             (make-propertyY 20 -10))
                                  #false
                                  0
                                  0))
                
                "mouse on the ball, and button pressed, the world will explode, the ball heading up"))
; STRATEGY: data decomposition on m : MouseEvent
(define (mouse-handler w x y m)
  (cond
    [(button-down? m) (if (and (mouse-within-ball? (world-ball w) x y)
                               (not (world-paused? w))
                               (not (hit-ground-next-tick? (ball-propertyY (world-ball w)))))
                          (set-explosion w)
                          w)]
    [else w]))


; pause-count-ball-img : World -> Image
; Render paused world state
; STRATEGY: function composition
(define (pause-count-ball-img w)
  (place-image PAUSE
               40
               10
               (count-ball-img w)))


; count-ball-img : World -> Image
; Render world state that is not paused
; STRATEGY: function composition
(define (count-ball-img w)
  (place-image (score-text (score w)) 
               270
               10
               (draw-ball (world-ball w) (world-explosion w))))


; world-ball : World -> Ball
; Returns a representation of the ball.
; STRATEGY: function composition
(begin-for-test
  (check-equal? (world-ball INITIAL-WORLD)
                (world-cball INITIAL-WORLD)
                "returns world"))
(define (world-ball w)
  (world-cball w))


; world-paused? : World -> Boolean
; Indicates whether the game is paused.
; STRATEGY: function composition
(define (world-paused? w)
  (world-paused w))


; ticks-since-click : World -> [0 10)
; Returns the number of ticks since the last explosion, if there's
; currently one. 0 means no explosion.
(begin-for-test
  (check-equal? (ticks-since-click INITIAL-WORLD)
                0
                "returns ticks since click"))
; STRATEGY: function composition
(define (ticks-since-click w)
  (world-explosion w))


; set-explosion : World -> World
; Sets the world in explostion state
; STRATEGY: function composition
(define (set-explosion w)
  (make-world (make-ball (ball-propertyX (world-ball w)) 
                         (explode-y (ball-propertyY (world-ball w))))
              (world-paused w) 
              (add1 (world-score w))
              1))


; score : World -> Score
; Returns the current score of the World
; STRATEGY: function composition
(define (score w)
  (world-score w))







; Ball Functions:____________________________

; score-text : Score -> Image
; Composes the count text image with the given score.
; STRATEGY: function composition
(define (score-text s)
  (beside COUNT (text (number->string s) 15 "black")))


; mouse-within-ball? : Ball Coordinate Coordinate -> Boolean
; Returns true if the mouse is within the ball(on the ball's edge excluded)
; STRATEGY: function composition
(define (mouse-within-ball? b x y)
  (< (+ (sqr(- x (ball-x b)))
        (sqr(- y (ball-y b))))     
     (sqr RADIUS)))


; next-ball : Ball -> Ball
; Computes next ball after one tick with the given ball b
; STRATEGY: data decomposition on b : Ball
(define (next-ball b)
  (make-ball (next-propertyX (ball-propertyX b))
             (next-propertyY (ball-propertyY b))))


; draw-ball : Ball Explosion -> Image 
; Renders the ball on the canvas.
; STRATEGY: function composition
(define (draw-ball b e)
  (place-image (draw-explosion-on-ball e)
               (ball-x b)
               (ball-y b)
               SCENE))


; ball-x : Ball -> Coordinate
; ball-y : Ball -> Coordinate
; Returns the x or y position of the given Ball.
; STRATEGY: data decomposition on b : Ball
(define (ball-x b)
  (propertyX-pos (ball-propertyX b)))
(define (ball-y b)
  (propertyY-pos (ball-propertyY b)))










; PropertyX Functions:____________________________

; next-propertyX : PropertyX -> PropertyX
; Computes next propertyX after one tick with the given propertyX curr-px
; STRATEGY: function composition
(define (next-propertyX curr-px)
  (if (>= (next-x curr-px) RIGHT-WALL)
      (make-propertyX RIGHT-WALL (* -1 (propertyX-v curr-px)))
      (if (<= (next-x curr-px) LEFT-WALL)
          (make-propertyX LEFT-WALL (* -1 (propertyX-v curr-px)))
          (make-propertyX (next-x curr-px) (propertyX-v curr-px)))))



; next-x : PropertyX -> Coordinate
; Computes next Coordinate in X direction after one tick with the given propertyX px
; STRATEGY: data decomposition on px : PropertyX
(define (next-x px)
  (+ (propertyX-pos px) (propertyX-v px)))










; PropertyY Functions:____________________________

; next-y : PropertyY -> Coordinate
; Computes next Coordinate in Y direction after one tick with the given propertyY py
; STRATEGY: data decomposition on py : PropertyY
(define (next-y py)
  (round/ε (+ (propertyY-pos py) (propertyY-v py) (* 0.5 ACCELERATION))))


; bounce-v : PropertyY -> Velocity
; Computes the Velocity when the ball hit the ground
; STRATEGY: data decomposition on py : PropertyY
(define (bounce-v py)
  (round/ε (sqrt (+ (expt (propertyY-v py) 2)
           (* 2 ACCELERATION (- DOWN-WALL (propertyY-pos py)))))))


; explode-ball : PropertyY -> PropertyY
; Changes properties in Y direction after the ball is exploded 
; STRATEGY: data decomposition on py : PropertyY
(define (explode-y py)
  (if (> (propertyY-v py) 0)
      (make-propertyY (propertyY-pos py) -10)
      (make-propertyY (propertyY-pos py) (- (propertyY-v py) 10))))


; next-propertyY : PropertyY -> PropertyY
; Computes next propertyY in Y direction after one tick with the given propertyY py
; STRATEGY: data decomposition on py : PropertyY
(define (next-propertyY py)
  (if (hit-ground-next-tick? py) 
      (make-propertyY DOWN-WALL (* (- 0 BC) (bounce-v py)))
      (make-propertyY (next-y py) (next-v (propertyY-v py)))))


; hit-ground-next-tick? : PropertyY -> Boolean
; Returns true if the Current PropertyY will pass or flush the DOWN-WALL after one tick
; STRATEGY: function composition
(define (hit-ground-next-tick? curr-py)
  (>= (next-y curr-py) DOWN-WALL))











; Explostion Function _____________________

; next-explosion : Explostion -> Explosion
; Computes next explosion of the ball
(begin-for-test
  (check-equal? (next-explosion 0)
                0
                "0")
  (check-equal? (next-explosion 8)
                9
                "8")
  (check-equal? (next-explosion 1)
                2
                "1"))
; STRATEGY: function composition
(define (next-explosion e)
  (if (<= 1 e 8) (add1 e) 0))


; draw-explosion-on-ball : Explosion -> Image 
; Renders explosion effect on the ball
; STRATEGY: function composition
(define (draw-explosion-on-ball e)
  (place-image (radial-star 12 
                            (* e INNER-EXPAND) 
                            (* e OUTER-EXPAND)
                            "solid" 
                            "yellow")
               RADIUS
               RADIUS
               BALL-IMG))





; Helper Functions _____________________

; round/ε : Real -> Real
; Rounds x to within ε precision
; STRATEGY: function composition
(define (round/ε x)
  (exact->inexact (* (inexact->exact (round (/ x ε))) ε)))



; String -> Boolean
; Returns true if the key pussed is 'p'.
; STRATEGY: function composition
(define (pause? ke)
  (string=? ke "p"))

; button-down? : String -> Boolean
; Returns trun if the mouse button is pushed down
; STRATEGY: function composition
(define (button-down? mo)
  (string=? "button-down" mo))


; next-v : Velocity -> Velocity
; Computes next velocity in Y direction after one tick with the given Velocity curr-v
; STRATEGY: function composition
(define (next-v curr-v)
  (+ curr-v ACCELERATION))





; Main Function _____________________

;(run INITIAL-WORLD)







; ================= Alternate Data Definition =================




;alternative data definition 1
 
; data definition: Velocity PropertyX PropertyY Score Explosion remains the same



; (define-structure ball [propertyX propertyY pause score explosion])
; A Ball is a (make-ball PropertyX PropertyY Boolean Score Explosion)
; INTERP: propertX propertyY represents the ball's position and velocity, pause
;         represents if the world game is paused, score represents the clicks since
;         the ball last hit ground, explosion represents ticks since the ball exploded,
;         0 means no explosion.

; A world is a Ball, representing the current world state.

; pros:
;	(1)Easier to control the world through only the Ball
;	(2)Easier to understand
;	(3)Less nested struture

; cons:
;	(1)Less capibility to extend this program
;	(2)Need many helper functions to transfer between World and Ball







;alternative data definition 2
 
; data definition: Velocity Score Explosion remains the same

; (define-struct ball [position velocity]
; A Ball is a (make-ball Position Velocity)
; INTERP: Position represents the ball's X and Y Coordinates, Velocity represents 
;         ball's velocity in Y direction

; (define-struct world [ball pause score explosion])
; A world is a Ball, representing the current world state.
; INTERP:  Ball represents the moving ball; Boolean means whether the current world
;         is paused; Score represents the clicks since last time hit ground; Explosion
;         represents the ticks since last explosion, 0 means no explostion.

; pros:
;	(1)Easier to get ball's X and Y coordinate which helps rendering the ball
;	(2)Easier to understand, because X and Y positions are grouped together.

; cons:
;	(1)Less capibility to extend this program
;	(2)More difficult to compute the ball's velocity and position, things get complicated
;  (3)Many helper functions will be needed, harder to read the program.

