;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname walls) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")

(require rackunit)

(require 2htdp/image)

(require 2htdp/universe)

(define TIME-ON-TASK 8) ; hours


; provides ______________

(provide INITIAL-WORLD)

(provide next-world)

(provide key-handler)

(provide mouse-handler)

(provide world-balls)

(provide make-ball)

(provide replace-balls)

(provide ball-x)

(provide ball-y)

;(provide end?)

;(provide score)

(provide level)











; constants _______________

(define WIDTH 400)

(define HEIGHT 400)

(define MT (empty-scene WIDTH HEIGHT))

(define RADIUS 20)

(define BALL-IMG (circle RADIUS "solid" "blue"))

(define WALL-WIDTH 4)

(define WALL-MAX-GROWTH 10)

(define WALL-MIN-GROWTH 1)

(define ALPHA 100)

(define BASE-GOAL 50)

(define MAX-GOAL 90)

(define END-IMG (text "GAME OVER!" 25 "red"))

(define MAX-VELOCITY 10)

(define MIN-VELOCITY 1)

(define FONT-SIZE 20)









;data definition ___________________



; A Speed is in either one of:
; - [-10, -1]
; - [1, 10]
; INTERP: positive means Speed toward positive direction,
;         vice versa.
(define sp1 -10) ; speed is 10 in negative X/Y direction, tick/pixels
(define sp2 5)   ; speed is 5 in positive X/Y direction, tick/pixels





(define-struct velocity [vx vy])
; A Velocity is a (make-velocity Speed Speed)
; INTERP: vx, vy represents the current phycial velocity of the ball along 
;         X or Y Axis; Positive means the ball is now moving in X/Y Coordinate's
;         positive direction; Negtive means toward oppsite direction.
(define ve1 (make-velocity sp2 sp1)) ; x direction speed is 5 tick/pixels
                                     ; y direction speed is -10 tick/pixels
; TEMPLATE:
; velocity-fn : Ball -> ???
(define (velocity-fn b)
  (... (velocity-vx b) ... (velocity-vy b) ...))





(define-struct iball [po v])
; A Ball is a (make-struct Posn Velocity)
; INTERP: posn po represents the ball's x and y Coordinates while velocity v represents 
;         the ball's Speed along x and y Coordinate.
(define b1 (make-iball (make-posn 100 100)  ; ball's position is (100, 100)
                        ve1))               ; ball's veclocity is (5, -10)
; TEMPLATE:
; iball-fn : Ball -> ???
(define (iball-fn b)
  (... (iball-po b) ... (iball-v b) ...))





; A ListOfBall is a ListOf<Ball>
; INTERP: Represents tha balls in the current world
(define lob1 (list b1))






(define-struct area [l r u d reachable])
; A Area is a (make-area Coordinate Coordinate Coordinate Coordinate Boolean)
; INTERP: l represents the area's left bound, while r represents right,
;         u for up, d for down
(define INITIAL-CANVAS (make-area 0 400 0 400 #true)) ; initial canvas

; TEMPLATE:
; area-fn : Area -> ???
(define (area-fn b)
  (... (area-l b) ... (area-r b) ...
       (area-u b) ... (area-d b) ...))




; A ListOfArea is a ListOf<Area>
; INTERP: Areas in this list reprensts the current structure of the canvas
;         that is, how the canvas is partioned by the walls. Every sub-area
;         represents a block.
(define INITIAL-AREAS (list INITIAL-CANVAS)) ; a list that only contains the initial canvas 





; A Direction is one of:
;  - "horizontal"
;  - "vertical"
; INTERP: horizontal means the wall is now extending in horizontal direction,
;         the same is for vertical
(define H "horizontal") ; horizontal
(define V "vertical")   ; vertical

; TEMPLATE:
; direction-fn : Direction -> ???
;(define (direction-fn d)
;  (cond
;    [(horizontal? d) ...]
;    [(vertical? d) ...]))




(define-struct wall [h1 h2 dir ac1 ac2])
; A Wall is a (make-wall Posn Posn Direction Boolean)
; INTERP: h1, h2 represents position of the two ends of the wall,
;         direction represents whether the wall is vertical or horizontal,
;         active means whether the wall is now active, or growing.
(define w0 (make-wall (make-posn 100 100)        ; a new started wall at (100, 100), active
                      (make-posn 102 100)
                      H
                      #true #true))
(define w2 (make-wall (make-posn 300 0)        
                      (make-posn 300 400)
                      V
                      #false #false))

(define WU (make-wall (make-posn 0 0)            ; upper bound of the whole canvans
                      (make-posn WIDTH 0)
                      H
                      #false #false))

(define WD (make-wall (make-posn 0 HEIGHT)       ; down bound of the whole canvans
                      (make-posn WIDTH HEIGHT)
                      H
                      #false #false))

(define WL (make-wall (make-posn 0 0)            ; left bound of the whole canvans
                      (make-posn 0 HEIGHT)
                      V
                      #false #false))

(define WR (make-wall (make-posn WIDTH 0)        ; right bound of the whole canvans
                      (make-posn WIDTH HEIGHT)
                      V
                      #false #false))
; TEMPLATE:
; wall-fn : Wall -> ???
(define (wall-fn w)
  (... (wall-h1 w) ... (wall-h2 w) ...
       (wall-dir w) ... (wall-ac1 w) ...(wall-ac2 w) ...))
                                       




; A ListOfWall is a ListOf<Wall>
; INTERP: the walls in the list represents a group of wall
(define INITIAL-WALLS (list w0 w2 WU WD WL WR))





(define-struct world [areas iballs walls level dir])
; A World is a (make-world ListOfBall ListOfArea ListOfWall NonNegInt)
; INTERP: world represents the current world state. areas means how many sub-areas
;         in this world, balls means how many balls, the same for walls.
;         level represents the current world's level.

(define world1 (make-world INITIAL-AREAS lob1 INITIAL-WALLS 1 H)) ; a world

; TEMPLATE:
; world-fn : World -> ???
(define (world-fn w)
  (... (world-areas w) ... (world-balls w) ...
       (world-walls w) ... (world-level w) ...))






; PlaceItem is Function
; [Item Image -> Image]
; INTERP: this function place one item on the given image, then
;         construct a new image






; A Item is one of: 
;  - Ball
;  - Wall
;  - Area
; INTERP: item can represents three different data: Ball, Wall, Area
(define it1 b1)




















; #World Funcitons _______________________



; run : World -> World
(define (run w)
  (big-bang w
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)
            (on-draw render)))


            ;(stop-when end? render-last)))




; next-world : World -> World  [areas iballs walls level dir]
(define (next-world w)
  (make-world (world-areas w)
              (world-iballs w)
              (next-wall (world-walls w))
              (world-level w)
              (world-dir w)))





; key-handler : World KeyEvent -> World
(define (key-handler w ke)
  (cond
    [(string=? "space" ke) w]
    [else w]))



; mouse-handler : World MouseEvent -> World
(define (mouse-handler w x y m)
  w)





; render : World -> Image
(define (render w)
  (local (;
          ;
          (define (draw-balls bs bkg)
            (foldr (place-item item-image ball-x ball-y) bkg bs))
          
          ;
          ;
          (define (draw-walls ws bkg)
            (foldr (place-item item-image wall-x wall-y) bkg ws))
          
          
          (define unreachables (filter (lambda (a) (not(area-reachable a)))
                                       (world-areas w)))
          ;
          ;
          (define (draw-areas as bkg)
            (foldr (place-item item-image area-x area-y) bkg unreachables))
          
          ;
          ;
          (define (draw-dir-text dir bkg)
            (place-image (text dir FONT-SIZE "black") 
                         (/ WIDTH 2) (- HEIGHT 20)  bkg)))
    
    
    (draw-dir-text (world-dir w) 
                   (draw-walls (world-walls w)
                               (draw-balls (world-balls w) 
                                           (draw-areas (world-areas w) MT))))))





















; world-balls : World -> ListOf<Ball>
; Returns all the balls in the given world.
(define (world-balls w)
  (world-iballs w))
 











 
; replace-balls : World ListOf<Ball> -> World
; Replaces the Balls in the given World with the given Balls.
(define (replace-balls w lob)
  (make-world (world-areas w)
              lob
              (world-walls w)
              (world-level w)))





; score : World -> Natural
; Returns the current score.
 (define (score w)...)




 
; level : World -> Natural
; Returns the current level.
 (define (level w)
   (world-level w))



 
 

 
 
 

 
; #Ball Functions __________________________________________________



; ball-x : Ball -> Coordinate
; Returns the x position of the Ball's center.
(define (ball-x b)
  (posn-x (iball-po b)))
 





; ball-y : Ball -> Coordiate
; Returns the y position of the Ball's center.
(define (ball-y b)
  (posn-y (iball-po b)))















; #Wall Functions __________________________________________________

; wall-x
(define (wall-x w)
  (/ (+ (posn-x (wall-h1 w))
        (posn-x (wall-h2 w)))
     2))

(define (wall-y w)
  (/ (+ (posn-y (wall-h1 w))
        (posn-y (wall-h2 w)))
     2))




; Wall -> Image
(define (wall-image w)
  (if (horizontal? (wall-dir w))
      (rectangle (abs (- (posn-x (wall-h1 w)) (posn-x (wall-h2 w))))
                 4 "solid" "brown")
      (rectangle 4 (abs (- (posn-y (wall-h1 w)) (posn-y (wall-h2 w))))
                 "solid" "brown")))


(define (wall-active w)
  (or (wall-ac1 w) (wall-ac2 w)))



; next-wall : ListOfWall -> ListOfWall
(define (next-wall ws) 
  (local (
          (define head (first ws)))
    (if (not (wall-active head)) 
        ws
        (cons (next-active-wall head (rest ws)) (rest ws)))))



; replace-active : Wall -> ListOfWall
; data decompostion on Wall : w
(define (next-active-wall w ws)
  (cond 
    [(horizontal? (wall-dir w)) (next-h-wall w ws)]
    [(vertical? (wall-dir w)) (next-v-wall w ws)]))



(define (next-h-wall w ws)
  (local(
         (define ppd-ws (perpendicular H ws))
         (define h1s (next-speed (wall-ac1 w) (wall-ac2 w)))
         (define h2s (next-speed (wall-ac2 w) (wall-ac1 w)))
         (define xs (map (lambda (p) (posn-x (wall-h1 p)))
                         ppd-ws))
         (define h1x (next-tick-wall (posn-x (wall-h1 w)) xs (- 0 h1s)))
         
         (define h2x (next-tick-wall (posn-x (wall-h2 w)) xs h2s))
         
         ;
         (define (create-wall w)
           (make-wall (make-posn h1x (wall-y w))
                      (make-posn h2x (wall-y w))
                      (wall-dir w)
                      (not (member? h1x xs))
                      (not (member? h2x xs)))))
    
    (create-wall w)
    ))



; Boolean Boolean -> NonNegInt
(define (next-speed ac1 ac2)
  (if (and ac1 ac2)
      WALL-MAX-GROWTH
      (if ac1 WALL-MIN-GROWTH 0)))




; Real ListOfReal -> Real
(begin-for-test
  (check-equal? (next-tick-wall 100 (list 0 110 200) 16)
                110
                ""))
(define (next-tick-wall x xs speed)
  (local(
         (define crossed-wall (crossed x xs speed))
         ; 
         (define (find-next x xs speed)
           (if (empty? crossed-wall)
               (+ x speed)
               (find-closest x crossed-wall)))
         )
    (find-next x xs speed)
    ))



(define (crossed x xs speed)
  (filter (lambda (n) (cross? x speed n))
          xs))




(define (cross? start speed line)
  (or (< start line (+ start speed))
      (> start line (+ start speed))))




; Real ListOfReal -> Real
(define (find-closest x xs)
  (local (;
          ;
          (define (to-pair x xs)
            (map (lambda (n) (make-posn n (abs (- x n))))
                 xs))
          ;
          (define sorted (sort (to-pair x xs)
                               (lambda (p1 p2) (< (posn-y p1) (posn-y p2)))))
          )
    (posn-x (first sorted))))










(define (next-v-wall w ws)
  ...)




; Direction ListOfWall -> ListOfWall
(define (perpendicular dir ws)
  (cond
    [(horizontal? dir) (filter (wall-filter wall-dir vertical?)
                               ws)]
    [(vertical? dir) (filter (wall-filter wall-dir horizontal?)
                               ws)]))




; [X -> Y] [Z -> Boolean] -> Function
(define (wall-filter property condition)
  (lambda (w) (condition (property w))))


; notactives : ListOfWall -> ListOfWall
(define (notactives ws)
  (filter (wall-filter wall-active false?)
                               ws))
;(actives INITIAL-WALLS)



; #Area Functions __________________________________________________

; Area -> Image
(define (area-image a)
  (rectangle (- (area-r a) (area-l a))
             (- (area-d a) (area-u a))
             "solid" "yellow"))





; area-x : 
; area-y : 
(define (area-x a)
  (/ (+ (area-r a) (area-l a))
     2))

(define (area-y a)
  (/ (+ (area-d a) (area-u a))
     2))

; unreachables : ListOfArea -> ListOfArea
(define (unreachables as)
  (filter (lambda (a) (not (area-reachable a))) 
          as))












; Direction Functions __________________________________________________


; horizontal? : Direction -> Boolean
; vertical? : Direction -> Boolean
; Returns true if the direction d is horizontal or vertical
(begin-for-test
  (check-true (horizontal? H)
              "horizontal")
  (check-false (vertical? H)
              "vertical"))
(define (horizontal? d)
  (string=? d H))
(define (vertical? d)
  (string=? d V))


















; Item Functions __________________________________________________

; [Item -> Image] [Item -> Coordinate] [Item -> Coordinate] -> PlaceItem
(define (place-item item-image item-x item-y)
  (lambda (one-item scene)
    (place-image (item-image one-item)
                 (item-x one-item) 
                 (item-y one-item) 
                 scene)))





; Item -> Image
(define (item-image item)
  (cond
    [(iball? item) BALL-IMG]
    [(wall? item) (wall-image item)]
    [(area? item) (area-image item)]))

















; Helper Functions __________________________________________________


; make-ball : Coordinate Coordinate Real Real -> Ball
; Returns a Ball with center at (x,y), with the given velocities.
; A positive x velocity is in the x-increasing direction and vice versa.
; The y velocity is similar.
(define (make-ball x y vx vy)
  (make-iball (make-posn x y)
              (make-velocity vx vy)))



; random-balls : NonNegInt -> ListOfBall
(define (random-balls level)
  (local (
          (define horizontal-wall (- WIDTH (+ (* 2 RADIUS) WALL-WIDTH)))
          (define vertical-wall (- HEIGHT (+ (* 2 RADIUS) WALL-WIDTH)))
          (define offset (+ RADIUS (/ WALL-WIDTH 2)))
          ;
          ;
          (define (random-ball level)
            (make-iball (random-posn horizontal-wall vertical-wall RADIUS offset)
                        (random-velocity MAX-VELOCITY MIN-VELOCITY))))
  
    (if (= 1 level)
        (list (random-ball level))
        (cons (random-ball level)
              (random-balls (sub1 level))))))



; random-posn : Coordinate Coordinate NonNegInt NonNegInt
; Returns a random posn within a width x height canvas.
; WHERE: the returned posn satisfies not going off the give
;        canvas
(begin-for-test
  (check-true (posn? (random-posn 300 300 10 5))
              "check for random posn"))
; STRATEGY: function composition
(define (random-posn width height interval offset)
  (make-posn 
   (+ offset (random (+ width 1)))
   (+ offset (random (+ height 1)))))





; random-velocity : 
; WHERE: the returned posn satisfies not going off the give
;        canvas
; STRATEGY: function composition
(define (random-velocity max min)
  (make-velocity
   (* (random-sign max) (+ (random max) min))
   (* (random-sign max) (+ (random max) min))))





; Real -> Real
(define (random-sign max)
  (if (> (random max) (/ max 2))
      -1
      1))






; velocity-legal? : velocity -> Boolean
(begin-for-test
  (check-true (velocity-illegal? (make-velocity 0 0) -1 1)
               ""))
(define (velocity-illegal? velocity start end)
  (and (< start (velocity-vx velocity) end)
       (< start (velocity-vy velocity) end)))













(define INITIAL-BALLS (random-balls 1))

(define INITIAL-WORLD (make-world INITIAL-AREAS INITIAL-BALLS INITIAL-WALLS 1 V))

; Main Function ____________________________________________

(run INITIAL-WORLD)



;(perpendicular H INITIAL-WALLS)