;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname in-classs-exercise) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
; In-class exercise: Moving Ball
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

; A Pixel is a NonNegReal, representing a canvas distance

; Canvas constans
(define S-WIDTH 400)                          ;pixels
(define S-HEIGHT 400)                         ;pixels
(define SCENE (empty-scene S-WIDTH S-HEIGHT)) ;Image
(define X-CENTER (/ (image-width SCENE) 2))   ;Coordinate
(define Y-CENTER (/ (image-height SCENE) 2))  ;Coordinate
(define END-TEXT-IMG (text "THE END" 24 "red"));Image

; Ball definition ----------------------
; A Ball is a Coordinate, representing the Ball's x position

; Ball constant
(define RADIUS 40)                     ; pixels
(define BALL-COLOR "red")              ; String  
(define BALL-OUTLINE "solid")          ; String
(define BALL-IMG (circle RADIUS BALL-OUTLINE BALL-COLOR)) ; Image
(define INIT-BALL X-CENTER)            ; Coordinate
(define VELOCITY 5)                    ; pixels/tick
(define BALL-LEFT-STEP-SIZE 10)        ; pixels
(define END-POINT (- S-WIDTH RADIUS))  ;Coordinate


; World definition ------------------------
; A World is a Ball
; INTEREP: represent a ball on the canvas
(define INIT-WORLD INIT-BALL)
(define LAST-WORLD END-POINT)

; World functions -------------------------


; run : World -> World
; Starts the simulation
(define (run init-world)
  (big-bang init-world
          [on-tick next-world]
          [to-draw draw-on-scene]
          [stop-when reach-end? draw-end-text]
          [on-key key-handler]))


; next-world : World -> World
; Computes the next World state after w
(begin-for-test
  (check-equal? (next-world INIT-WORLD) (+ INIT-WORLD VELOCITY)
                "Compute 2nd World state"))
(define (next-world w)
  (next-ball w))

; draw-on-scene: World -> Image
; Renders the currents World state w
(begin-for-test 
  (check-equal? (draw-on-scene INIT-WORLD)
                (place-image BALL-IMG X-CENTER Y-CENTER SCENE)
                "draw init world"))
(define (draw-on-scene w)
  (moving-ball w))


; reach-end?: World -> Boolean
; Returns true if current world w is the last world
(begin-for-test
  (check-true (reach-end? (add1 END-POINT)) "simulation ends")
  (check-false (reach-end? (add1 INIT-WORLD)) "simulation continues"))
(define (reach-end? w)
  (ball-passed-end? w))


; draw-end-text: World -> Image
; draw the last scene of the World state w
(begin-for-test
  (check-equal? (draw-end-text LAST-WORLD)
                (place-image END-TEXT-IMG X-CENTER Y-CENTER (draw-on-scene LAST-WORLD))
                "world ends with text 'THE END'"))
(define (draw-end-text w)
  (place-image END-TEXT-IMG X-CENTER Y-CENTER (draw-on-scene w)))


; key-handler : World KeyEvent -> World
; Computes the next world state in response to key press ke
; - "left" moves ball the left by 10 pixels
; - other keys leave w unchanged
(begin-for-test
  (check-equal? (key-handler INIT-WORLD "left")
                (- INIT-WORLD BALL-LEFT-STEP-SIZE)
                "valid key, the ball moves to left BALL-LEFT-STEP-SIZE pixels")
  (check-equal? (key-handler INIT-WORLD "right")
                INIT-WORLD
                "invalid key, world will not change"))
(define (key-handler w ke)
  (if (key=? ke "left") (move-ball-left w BALL-LEFT-STEP-SIZE) w))



; BALL functions ----------------------

; next-ball : Ball -> Ball
; Computes the next Ball position
(begin-for-test
  (check-equal? (next-ball -5) 0 "negative position")
  (check-equal? (next-ball 5) 10 "positive position")
  (check-equal? (next-ball END-POINT) (+ END-POINT VELOCITY) "invalid position"))
(define(next-ball b)
  (+ b VELOCITY))


; moving-ball: Ball -> Image
; place the Ball b on the Scene as with it's x Coordnite
(begin-for-test
  (check-equal? (moving-ball INIT-BALL)
                (place-image BALL-IMG INIT-BALL Y-CENTER SCENE)
                "draw init ball at center")
  (check-equal? (moving-ball END-POINT)
                (place-image BALL-IMG END-POINT Y-CENTER SCENE)
                "draw the last point of the ball"))
(define (moving-ball b)
  (place-image BALL-IMG b Y-CENTER SCENE))


; ball-passed-end? : Ball -> Boolean
; Return true if the ball passed the end point
(begin-for-test
  (check-true (ball-passed-end? (add1 END-POINT)) "ball off canvas")
  (check-false (ball-passed-end? INIT-BALL) "ball on canvas"))
(define (ball-passed-end? b)
  (> b END-POINT))


; move-ball-left : Ball Pixels -> Ball
; move the ball b to the left by dist pixels
(begin-for-test
  (check-equal? (move-ball-left INIT-BALL BALL-LEFT-STEP-SIZE)
                (- INIT-BALL BALL-LEFT-STEP-SIZE)
                "move the ball BALL-LEFT-STEP-SIZE pixels from center"))
(define (move-ball-left b dist)
  (- b dist))

(run INIT-WORLD)
