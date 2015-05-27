;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(define TIME-ON-TASK 8) ; hours

(provide initial-robot)
(provide robot-left)
(provide robot-right)
(provide robot-x)
(provide robot-y)
(provide robot-forward)

;(check-location "02" "robot.rkt")

; graphcial constants
(define L-BOUNDARY 15)
(define R-BOUNDARY 185)
(define U-BOUNDARY 15)
(define D-BOUNDARY 385)

; Direction constants:
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

; Position constants:
(define X1 100) ; Coordinate
(define Y1 100) ; Coordinate
(define X2 200) ; Coordinate
(define Y2 400) ; Coordinate
(define X3 185) ; Coordinate
(define Y3 385) ; Coordinate
(define X4 215) ; Coordinate
(define Y4 415) ; Coordinate
(define X5 15)  ; Coordinate
(define Y5 15)  ; Coordinate
(define X6 -10) ; Coordinate
(define Y6 -10) ; Coordinate

; Direction definition ———————————————————
; A Direction is a one of :
; - "left"
; - "right"
; - "up"
; - "down"
; INTERP:
;        Represents the current direction of the robot, if it
;        is "left" it means that the robot is now heading left,
;        and so on.

; TEMPLATE:
; direction-fn : Direction -> ???
;(define (direction-fn d)
;  (cond
;    [(LEFT? d) ...]
;    [(RIGHT? d) ...]
;    [(UP? d) ...]
;    [(DOWN? d) ...]))


; Position definition ———————————————————
; A Position is a make-posn(Coordinate, Coordinate)
; INTERP: Represents a dot's x coordinate and y coordinate


; Robot definition ———————————————————
; A Robot is a structure: make-robot(Position, Direction)
; INTERP: Position represents robot's currection position;
;         direction dictates where the robot is heading
(define-struct robot [posn dir])

; Robot constants: 
(define RLEFT (make-robot (make-posn X1 Y1) LEFT))   ; a robot heading left, completely inside room
(define RRIGHT (make-robot (make-posn X2 Y2) RIGHT)) ; a robot heading right, particaly inside room
(define RUP (make-robot (make-posn X3 Y3) UP))       ; a robot heading up, edge flush against wall
(define RDOWN (make-robot (make-posn X4 Y4) DOWN))   ; a robot heading down, completely outside room


; Robot Functions ———————————————————

; initial-robot : Coordinate Coordinate -> Robot
; Returns a Robot located at (x,y), facing up.
(begin-for-test
  (check-equal? (initial-robot X1 Y1)
                (make-robot (make-posn X1 Y1) UP) 
                "produce an initial robot whose initial location is (100,100) and facing up")
  (check-equal? (initial-robot X2 Y2)
                (make-robot (make-posn X2 Y2) UP) 
                "produce an initial robot whose initial location is (200,400) and facing up")
  (check-equal? (initial-robot X3 Y3)
                (make-robot (make-posn X3 Y3) UP) 
                "produce an initial robot whose initial location is (185,185) and facing up")
  check-equal? (initial-robot X4 Y4)
                (make-robot (make-posn X4 Y4) UP) 
                "produce an initial robot whose initial location is (215,415) and facing up")
(define (initial-robot x y)
  (make-robot (make-posn x y) UP))
 
; robot-left : Robot -> Robot
; robot-right : Robot -> Robot
; Returns a Robot like r, but turned either 90 degrees left or right.
; STRATAGY: function composition
(begin-for-test
  (check-equal? (robot-left RLEFT)
                (make-robot (robot-posn RLEFT) DOWN) 
                "heading left turn 90 degrees left will heading down")
  (check-equal? (robot-left RRIGHT)
                (make-robot (robot-posn RRIGHT) UP) 
                "heading right turn 90 degrees left will heading up")
  (check-equal? (robot-left RUP)
                (make-robot (robot-posn RUP) LEFT) 
                "heading up turn 90 degrees left will heading left")
  (check-equal? (robot-left RDOWN)
                (make-robot (robot-posn RDOWN) RIGHT) 
                "heading down turn 90 degrees left will heading right"))
(define (robot-left r)
  (make-robot (robot-posn r) (turn-left (robot-dir r))))

(begin-for-test
  (check-equal? (robot-right RLEFT)
                (make-robot (robot-posn RLEFT) UP) 
                "heading left turn 90 degrees right will heading up")
  (check-equal? (robot-right RRIGHT)
                (make-robot (robot-posn RRIGHT) DOWN) 
                "heading right turn 90 degrees right will heading down")
  (check-equal? (robot-right RUP)
                (make-robot (robot-posn RUP) RIGHT) 
                "heading up turn 90 degrees right will heading right")
  (check-equal? (robot-right RDOWN)
                (make-robot (robot-posn RDOWN) LEFT) 
                "heading down turn 90 degrees right will heading left"))
(define (robot-right r)
  (make-robot (robot-posn r) (turn-right (robot-dir r))))
 
; robot-x : Robot -> Coordinate
; robot-y : Robot -> Coordinate
; Returns the x or y component of the Robot's location.
(begin-for-test
  (check-equal? (robot-x RLEFT) 
                100
                "robot RLEFT's x Coordinate is 100")
  (check-equal? (robot-x RRIGHT) 
                200
                "this robot's x Coordinate is 200")
  (check-equal? (robot-x RUP) 
                185 
                "robot RUP's x Coordinate is 185")
  (check-equal? (robot-x RDOWN) 
                215
                "this robot's x Coordinate is 215"))
(define (robot-x r)
  (posn-x (robot-posn r)))

(begin-for-test
  (check-equal? (robot-y RLEFT) 
                100 
                "robot RLEFT's y Coordinate is 100")
  (check-equal? (robot-y RRIGHT) 
                400 
                "robot RLEFT's y Coordinate is 400")
  (check-equal? (robot-y RUP) 
                385 
                "robot RLEFT's y Coordinate is 385")
  (check-equal? (robot-y RDOWN) 
                415
                "robot RLEFT's y Coordinate is 415"))
(define (robot-y r)
  (posn-y (robot-posn r)))
 
; robot-forward : Robot NonNegReal -> Robot
; Returns a Robot like r, but moved forward by d pixels.  
; If the robot is inside the room and moving would put any part of the
; robot outside the room, the robot should stop at the wall that it's facing.
(begin-for-test
  (check-equal? (robot-forward (make-robot (make-posn X1 Y1) LEFT) 10)
                (make-robot (make-posn (- X1 10) Y1) LEFT)
                "move forwar 10 pixels toward left within the room")
  (check-equal? (robot-forward (make-robot (make-posn X1 Y1) LEFT) 1000)
                (make-robot (make-posn L-BOUNDARY Y1) LEFT)
                "move forwar 1000 pixels toward left , but confined in the room")
  (check-equal? (robot-forward (make-robot (make-posn X2 Y2) RIGHT) 10)
                (make-robot (make-posn (+ X2 10) Y2) RIGHT)
                "move forwar 10 pixels toward right freely")
  (check-equal? (robot-forward (make-robot (make-posn X1 Y1) RIGHT) 1000)
                (make-robot (make-posn R-BOUNDARY Y1) RIGHT)
                "move forward 1000 pixels toward right, but confined in the room")
  (check-equal? (robot-forward (make-robot (make-posn X3 Y3) UP) 10)
                (make-robot (make-posn X3 (- Y3 10)) UP)
                "move forwar 10 pixels")
  (check-equal? (robot-forward (make-robot (make-posn X3 Y3) UP) 1000)
                (make-robot (make-posn X3 U-BOUNDARY) UP)
                "RUP move forward 1000 pixels, but confined in the room")
  (check-equal? (robot-forward (make-robot (make-posn X4 Y4) DOWN) 1000)
                (make-robot (make-posn X4 (+ Y4 1000)) DOWN)
                "RDOWN move forward 1000 pixels freely")
  (check-equal? (robot-forward (make-robot (make-posn X1 Y1) DOWN) 1000)
                (make-robot (make-posn X1 D-BOUNDARY) DOWN)
                "move forward 1000 pixels, but confined in the room"))
(define (robot-forward r d)
  (make-robot (direction-forward (robot-dir r) (robot-posn r) d) (robot-dir r)))
 



; Direction Functions ———————————————————

; LEFT? : Direction -> Boolean
; Returns true if the Direction d is LEFT
(begin-for-test
  (check-true (LEFT? LEFT) "true")
  (check-false (LEFT? RIGHT) "false"))
(define (LEFT? d)
  (string=? d LEFT))

; RIGHT? : Direction -> Boolean
; Returns true if the Direction d is RIGHT
(begin-for-test
  (check-true (RIGHT? RIGHT) "true")
  (check-false (RIGHT? LEFT) "false"))
(define (RIGHT? d)
  (string=? d "right"))

; UP? : Direction -> Boolean
; Returns true if the Direction d is UP
(begin-for-test
  (check-true (UP? UP) "true")
  (check-false (UP? DOWN) "false"))
(define (UP? d)
  (string=? d "up"))

; DOWN? : Direction -> Boolean
; Returns true if the Direction d is DOWN
(begin-for-test
  (check-true (DOWN? DOWN) "true")
  (check-false (DOWN? UP) "false"))
(define (DOWN? d)
  (string=? d "down"))

; turn-left : Direction -> Direction
; turn-right : Direction -> Direction
; turn 90 degrees left or right based on current direction
; STRATAGY: data decomposition
(define (turn-left d)
  (cond
    [(LEFT? d) DOWN]
    [(RIGHT? d) UP]
    [(UP? d) LEFT]
    [(DOWN? d) RIGHT]))

(define (turn-right d)
  (cond
    [(LEFT? d) UP]
    [(RIGHT? d) DOWN]
    [(UP? d) RIGHT]
    [(DOWN? d) LEFT]))


; direction-forward : Directon Position NonNegReal -> Position
; Computes next position based on the current direction, current position and distance
; STRATAGY: data decomposition
(define (direction-forward dir posn d)
  (cond
    [(LEFT? dir) (left-move posn d)]
    [(RIGHT? dir) (right-move posn d)]
    [(UP? dir) (up-move posn d)]
    [(DOWN? dir) (down-move posn d)]))



; Position Functions ———————————————————

; between-up-down-bounday? : Position -> Boolean
; Returns true if the positon is between 
; U-BOUNDARY and D-BOUNDARY
(begin-for-test
  (check-true (between-up-down-bounday? (make-posn X1 Y1))
              "(100, 100) is between the two boundaries")
  (check-false (between-up-down-bounday? (make-posn X4 Y4))
              "(215, 415) is NOT between the two boundaries")
  (check-true (between-up-down-bounday? (make-posn X3 Y3))
              "(185, 385) is between the two boundaries")
  (check-true (between-up-down-bounday? (make-posn X5 Y5))
              "(15, 15) is between the two boundaries"))
(define (between-up-down-bounday? pos)
  (<= U-BOUNDARY (posn-y pos) D-BOUNDARY))


;between-left-right-bounday? : Position -> Boolean
; Returns true if the positon is between 
; L-BOUNDARY and R-BOUNDARY
(begin-for-test
  (check-true (between-left-right-bounday? (make-posn X1 Y1))
              "(100, 100) is between the two boundaries")
  (check-false (between-left-right-bounday? (make-posn X4 Y4))
              "(215, 415) is NOT between the two boundaries")
  (check-true (between-left-right-bounday? (make-posn X3 Y3))
              "(185, 385) is between the two boundaries")
  (check-true (between-left-right-bounday? (make-posn X5 Y5))
              "(15, 15) is between the two boundaries"))
(define (between-left-right-bounday? pos)
  (<= L-BOUNDARY (posn-x pos) R-BOUNDARY))


; right-to-left-boundary? : Position -> Boolean
; Returns true if the position pos is at the right side of the L-BOUNDARY
(begin-for-test
  (check-true (right-to-left-boundary? (make-posn X1 Y1))
              "(100, 100) is at right side of the L-BOUNDARY")
  (check-false (right-to-left-boundary? (make-posn X6 Y6))
              "(-10, -10) is at left side of the L-BOUNDARY")
  (check-true (right-to-left-boundary? (make-posn X5 Y5))
              "(15, 15) is at right side of the L-BOUNDARY"))
(define (right-to-left-boundary? pos)
  (>= (posn-x pos) L-BOUNDARY))


; left-to-right-boundary? : Position -> Boolean
; Returns true if the position pos is at the left side of the R-BOUNDARY
(begin-for-test
  (check-true (left-to-right-boundary? (make-posn X1 Y1))
              "(100, 100) is at left side of the R-BOUNDARY")
  (check-false (left-to-right-boundary? (make-posn X4 Y4))
              "(215, 415) is NOT at left side of the R-BOUNDARY")
  (check-true (left-to-right-boundary? (make-posn X3 Y3))
              "(185, 385) is at left side of the R-BOUNDARY"))
(define (left-to-right-boundary? pos)
  (<= (posn-x pos) R-BOUNDARY))


; down-to-up-boundary? : Position -> Boolean
; Returns true if the position pos is at the down side of the U-BOUNDARY
(begin-for-test
  (check-true (down-to-up-boundary? (make-posn X1 Y1))
              "(100, 100) is at down side of the U-BOUNDARY")
  (check-false (down-to-up-boundary? (make-posn X6 Y6))
              "(-10, -10) is NOT at down side of the U-BOUNDARY")
  (check-true (down-to-up-boundary? (make-posn X5 Y5))
              "(15, 15) is at down side of the U-BOUNDARY"))
(define (down-to-up-boundary? pos)
  (>= (posn-y pos) U-BOUNDARY))


; up-to-down-boundary? : Position -> Boolean
; Returns true if the position pos is at the up side of the D-BOUNDARY
(begin-for-test
  (check-true (up-to-down-boundary? (make-posn X1 Y1))
              "(100, 100) is at up side of the D-BOUNDARY")
  (check-false (up-to-down-boundary? (make-posn X4 Y4))
              "(215, 415) is NOT at up side of the D-BOUNDARY")
  (check-true (up-to-down-boundary? (make-posn X3 Y3))
              "(185, 385) is at up side of the D-BOUNDARY"))
(define (up-to-down-boundary? pos)
  (<= (posn-y pos) D-BOUNDARY))


; left-move : Position NonNegReal -> Position
; Moves the current position pos toward left for d pixels
(begin-for-test
  (check-equal? (left-move (make-posn X1 Y1) 10)
                (make-posn (- X1 10) Y1)
                "current pos (100, 100), left-move 10, (90, 100) then")
  (check-equal? (left-move (make-posn X1 Y1) 1000)
                (make-posn L-BOUNDARY Y1)
                "current pos (100, 100), left-move 1000, (15, 100) then
                edge flush aginst the wall")
  (check-equal? (left-move (make-posn X4 Y4) 1000)
                (make-posn (- X4 1000) Y4)
                "current pos (215, 415), left-move 1000, (-815, 415) then"))
(define (left-move pos d) 
  (cond
    [(and (between-up-down-bounday? pos)
          (right-to-left-boundary? pos))
     (make-posn (max L-BOUNDARY (- (posn-x pos) d))
                (posn-y pos))]
    [else (make-posn (- (posn-x pos) d) 
                     (posn-y pos))]))


; right-move : Position NonNegReal -> Position
; Moves the current position pos toward right for d pixels
(begin-for-test
  (check-equal? (right-move (make-posn X1 Y1) 10)
                (make-posn (+ X1 10) Y1)
                "current pos (100, 100), right-move 10, (110, 100) then")
  (check-equal? (right-move (make-posn X1 Y1) 1000)
                (make-posn R-BOUNDARY Y1)
                "current pos (100, 100), right-move 1000, (185, 100) then
                edge flush aginst the wall")
  (check-equal? (right-move (make-posn X4 Y4) 1000)
                (make-posn (+ X4 1000) Y4)
                "current pos (215, 415), right-move 1000, (1215, 500) then"))
(define (right-move pos d) 
  (cond
    [(and (between-up-down-bounday? pos)
          (left-to-right-boundary? pos))
     (make-posn (min R-BOUNDARY (+ (posn-x pos) d)) 
                (posn-y pos))]
    [else (make-posn (+ (posn-x pos) d) 
                     (posn-y pos))]))


; up-move : Position NonNegReal -> Position
; Moves the current position pos toward up for d pixels
(begin-for-test
  (check-equal? (up-move (make-posn X1 Y1) 10)
                (make-posn X1 (- Y1 10))
                "current pos (100, 100), up-move 10, (100, 90) then")
  (check-equal? (up-move (make-posn X1 Y1) 1000)
                (make-posn X1 U-BOUNDARY)
                "current pos (100, 100), up-move 1000, (100, 15) then
                edge flush aginst the wall")
  (check-equal? (up-move (make-posn X4 Y4) 1000)
                (make-posn X4 (- Y4 1000))
                "current pos (215, 415), up-move 1000, (215, -585) then"))
(define (up-move pos d) 
  (cond
    [(and (between-left-right-bounday? pos)
          (down-to-up-boundary? pos))
     (make-posn (posn-x pos)
                (max U-BOUNDARY  (- (posn-y pos) d)))]
    [else (make-posn (posn-x pos)
                     (- (posn-y pos) d))]))


; down-move : Position NonNegReal -> Position
; Moves the current position pos toward down for d pixels
(begin-for-test
  (check-equal? (down-move (make-posn X1 Y1) 10)
                (make-posn X1 (+ Y1 10))
                "current pos (100, 100), down-move 10, (100, 110) then")
  (check-equal? (down-move (make-posn X1 Y1) 1000)
                (make-posn X1 D-BOUNDARY)
                "current pos (100, 100), down-move 1000, (100, 385) then
                edge flush aginst the wall")
  (check-equal? (down-move (make-posn X4 Y4) 1000)
                (make-posn X4 (+ Y4 1000))
                "current pos (215, 415), down-move 1000, (215, 1415) then"))
(define (down-move pos d) 
  (cond
    [(and (between-left-right-bounday? pos)
          (up-to-down-boundary? pos))
     (make-posn (posn-x pos)
                 (min D-BOUNDARY (+ (posn-y pos) d)))]
    [else (make-posn (posn-x pos)
                      (+ (posn-y pos) d))]))


