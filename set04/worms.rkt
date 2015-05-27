;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worms) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(require "extras.rkt")

(require rackunit)

(require 2htdp/image)

(require 2htdp/universe)

(define TIME-ON-TASK 8) ; hours







; provides ____________

(provide INITIAL-WORLD)

(provide next-world)

(provide key-handler)

(provide end?)

(provide world-worm)

(provide create-worm)

(provide worm-length)

(provide worm-head-x)

(provide worm-head-y)

(provide replace-food)

(provide replace-worm)

(provide posns-overlap?)




; constants ___________

(define HEIGHT 300) ; Pixels

(define WIDTH 300)  ; Pixels

(define MT (empty-scene WIDTH HEIGHT)) ; IMAGE

(define DIAMETER 10); Pixels

(define RADIUS (/ DIAMETER 2)) ; Pixels

(define WORM-IMG (circle RADIUS "solid" "red"))   ; IMAGE

(define FOOD-IMG (circle RADIUS "solid" "green")) ; IMAGE

(define HIT-WALL (text "WORM HIT WALL! score : " 20 "black")) ; IMAGE

(define HIT-SELF (text "WORM HIT SELF! score : " 20 "black")) ; IMAGE

(define TICK-RATE (/ 1 4)) ; seconds/tick

(define LEFT-WALL RADIUS)             ; Coordinate

(define RIGHT-WALL (- WIDTH RADIUS))  ; Coordinate

(define UP-WALL RADIUS)               ; Coordinate

(define DOWN-WALL (- HEIGHT RADIUS))  ; Coordinate




; data definition ______________________________


; ListOfPosn definition
; A ListOfPosn is either
; – '() or
; – (cons Posn ListOfPosn)
(define lop1 '())             ; empty list
(define lop2
   (list (make-posn 200 200)  ; list with two elements
         (make-posn 100 200)))

; TEMPLATE:
; loo-fn : w -> ???
; (define (loo-fn lp)
;   (cond
;     [(empty? lp) ...]
;     [else (... (first lp)
;                (loo-fn (rest lp)))]))





; Direction definition
; A Direction is a one of :
; - "left"
; - "right"
; - "up"
; - "down"
; INTERP:
;        Represents the current direction of the worm, if it
;        is "left" it means that the worm is now heading left,
;        and so on.
(define R "right") ; right
(define L "left")  ; left
(define U "up")    ; up
(define D "down")  ; down

; TEMPLATE:
; direction-fn : Direction -> ???
;(define (direction-fn d)
;  (cond
;    [(left? d) ...]
;    [(right? d) ...]
;    [(up? d) ...]
;    [(down? d) ...]))





; Worm definition
(define-struct worm [head body dir])
; A Worm is a (make-worm Posn ListOfPosn Direction)
; INTERP: lst represents the worm's body, which is consisted by a series of positions
;         while dir represents the direction where the worm is now heading.
(define INITIAL-WORM (make-worm (make-posn RADIUS RADIUS) '() D))  ; initial worm

; TEMPLATE:
; worm-fn : Food -> ???
;(define (worm-fn wm)
;  (... (worm-head wm) ... (worm-body wm) ...))





; Food definition
; A Food is a (make-posn Coordinate Coordinate)
; INTERP: x, y represent the x and y coordinate of the food
(define FD (make-posn 150 150)) ; initial food

; TEMPLATE:
; food-fn : Food -> ???
;(define (food-fn fd)
;  (... (food-x fd) ... (food-y fd) ...))





; World definition
(define-struct world [wm food])
; A World is a (make-world Worm Food)
; INTERP: wm represents the Worm in the world, fd represents the food in the world
(define INITIAL-WORLD (make-world INITIAL-WORM FD)) ; initial world

; TEMPLATE:
; world-fn : World -> ???
;(define (world-fn wd)
;  (... (world-worm wd) ... (world-food wd) ...))



















; World Functions _______________________________


; run : world -> Image
; Starts a worm eating food game, each time the worm 'eat' one 
; piece of food, the worm grows one segments. When the worm hit 
; wall or hit itself, the game ends.
; STRATEGY: function composition
(define (run w tick-rate)
 (big-bang w
            (on-tick next-world tick-rate)
            (on-key key-handler)
            (on-draw render)
            (stop-when end? render-last)))



; next-world : World -> World
; Computes next world after one tick
(begin-for-test
  (check-equal? (next-world INITIAL-WORLD)
                (make-world (make-worm (make-posn 5 15)
                                       '()
                                       D)
                            (world-food INITIAL-WORLD))
                "next world after inital world"))
; STRATAGY: data decomposition on  World : w
(define (next-world w)
  (replace-food (replace-worm w 
                              (worm-move (world-worm w) 
                                         (world-food w)))
                (next-food w)))




; key-handler : World KeyEvent -> World
; Returns a new World after one of keys: 'left' 'right' 'up' 'down' was pressed
(begin-for-test
  (check-equal? (key-handler INITIAL-WORLD "left")
                (make-world (turn-left (world-worm INITIAL-WORLD) FD) (world-food INITIAL-WORLD))
                "press key 'left' at the initial world")
  (check-equal? (key-handler INITIAL-WORLD "right")
                (make-world (turn-right (world-worm INITIAL-WORLD) FD) (world-food INITIAL-WORLD))
                "press key 'right' at the initial world")
  (check-equal? (key-handler INITIAL-WORLD "up")
                (make-world (turn-up (world-worm INITIAL-WORLD) FD) (world-food INITIAL-WORLD))
                "press key 'up' at the initial world")
  (check-equal? (key-handler INITIAL-WORLD "down")
                (make-world (turn-down (world-worm INITIAL-WORLD) FD) (world-food INITIAL-WORLD))
                "press key 'down' at the initial world")
  (check-equal? (key-handler INITIAL-WORLD "a")
                INITIAL-WORLD
                "press key 'down' at the initial world"))
; STRATAGY: data decomposition on KeyEvent : ke
(define (key-handler wd ke)
  (cond
    [(left? ke) (make-world (turn-left (world-worm wd) (world-food wd)) (next-food wd))]
    [(right? ke) (make-world (turn-right (world-worm wd) (world-food wd)) (next-food wd))]
    [(up? ke) (make-world (turn-up (world-worm wd) (world-food wd)) (next-food wd))]
    [(down? ke) (make-world (turn-down (world-worm wd) (world-food wd)) (next-food wd))]
    [else wd]))




; World -> Image
; Renders the current world state into image
(begin-for-test
  (check-equal? (render INITIAL-WORLD)
                (place-image FOOD-IMG
               (posn-x (world-food INITIAL-WORLD))
               (posn-y (world-food INITIAL-WORLD))
               (draw-posn (worm->list (world-worm INITIAL-WORLD))))
                "renders the initial world"))
; STRATAGY: data decomposition on  World : w 
(define (render w)
  (place-image FOOD-IMG
               (posn-x (world-food w))
               (posn-y (world-food w))
               (draw-posn (worm->list (world-worm w)))))





; render-last : World -> Image
; Renders the last world image
(begin-for-test
  (check-equal? (render-last (make-world (make-worm (make-posn 0 15)
                                                    '()
                                                    D)
                                         (world-food INITIAL-WORLD)))
                (end-image HIT-WALL (make-world (make-worm (make-posn 0 15)
                                                    '()
                                                    D)
                                         (world-food INITIAL-WORLD)))
                "world ends with worm hitting wall")
  (check-equal? (render-last (make-world (make-worm (make-posn 5 15)
                                                    (list (make-posn 5 15))
                                                    D)
                                         (world-food INITIAL-WORLD)))
                (end-image HIT-SELF (make-world (make-worm (make-posn 5 15)
                                                    (list (make-posn 5 15))
                                                    D)
                                         (world-food INITIAL-WORLD)))
                "world ends with worm hitting itself"))
; STRATAGY: data decomposition on World : w
(define (render-last w)
  (if (hit-wall? (world-worm w))
      (end-image HIT-WALL w)
      (end-image HIT-SELF w)))




; end-image : Image World -> Image
; Draws the last image with information of score and what kind of end:
; hitting wall or hitting self
(begin-for-test
  (check-equal? (end-image HIT-WALL INITIAL-WORLD)
                (place-image (score-text HIT-WALL (score INITIAL-WORLD))
                             150 150
                             (render INITIAL-WORLD))
                "example of end image"))
; STRATEGY: function composition
(define (end-image txt w)
  (place-image (score-text txt (score w))
               150 150
               (render w)))



; score-text : NonNegInt Image -> Image
; Composes the count text image with the given score.
(begin-for-test
  (check-equal? (score-text HIT-WALL 1)
                (beside HIT-WALL (text (number->string 1) 20 "red"))
                "WORM HIT WALL SCORE 1"))
; STRATEGY: function composition
(define (score-text txt s)
  (beside txt (text (number->string s) 20 "red")))




; score : World -> NonNegInt
; Returns the how many food the worm have eaten
(begin-for-test
  (check-equal? (score INITIAL-WORLD)
                0
                "inital world has score zero."))
; STRATAGY: data decomposition on Worm : w
(define (score w)
  (length (worm-body (world-worm w))))




; end? : World -> Boolean
; Returns true if either the worm hit wall or hit itself
(begin-for-test
  (check-equal? (end? (make-world (make-worm (make-posn 0 15)
                                             '()
                                             D)
                                  (world-food INITIAL-WORLD)))
                #true
                "")
  (check-equal? (end? (make-world (make-worm (make-posn 5 15)
                                             (list (make-posn 5 15))
                                             D)
                                  (world-food INITIAL-WORLD)))
                #true
                ""))
; STRATAGY: data decomposition on Worm : w
(define (end? w)
  (or (hit-wall? (world-worm w))
      (hit-self? (world-worm w))))





; next-food : World -> Food
; Computes the food after one tick, if the worm have eaten the food then randomly 
; create another food that do not overlap with any part of the worm, or else the 
; food remains the same
(begin-for-test
  (check-true (posn? (next-food INITIAL-WORLD))
                "food not eatean, food remain the same")
  (check-true (posn? (next-food (make-world (make-worm (make-posn 5 15)
                                            '()
                                            D)
                                (make-posn 5 15))))
                "food eatean by worm, create another food"))
; STRATAGY: data decomposition on World : w
(define (next-food w)
  (if (overlap? (worm-head (world-worm w)) (world-food w))
      (random-food (worm->list (world-worm w)))
      (world-food w)))





; replace-worm : World Worm -> World
; Replaces *only the positions* of the Worm in World w with the positions
; of the given worm. Any other Worm properties in the resulting World 
; should be the same as in the input World.
; WHERE: The Worm does not overlap with the food.
(begin-for-test
  (check-equal? (replace-worm INITIAL-WORLD INITIAL-WORM)
                INITIAL-WORLD
                "replace the initial world with inital worm"))
; STRATAGY: data decomposition on World : w
(define (replace-worm w worm)
  (make-world worm (world-food w)))




; replace-food : World Posn -> World
; Inserts a piece of food into the world at the given Coordinates,
; replacing the existing food.
; WHERE: The food does not overlap with any of the worm's segments.
(begin-for-test
  (check-equal? (replace-food INITIAL-WORLD FD)
                INITIAL-WORLD
                "replace the initial world with inital food"))
; STRATAGY: data decomposition on World w
(define (replace-food w food)
  (make-world (world-worm w) food))




; world-worm : World -> Worm
; Returns a representation of the Worm in the game.
(begin-for-test
  (check-equal? (world-worm INITIAL-WORLD)
                INITIAL-WORM
                "worm of the initial world"))
; STRATEGY: function composition
(define (world-worm w)
  (world-wm w))























; ListOfPosn Functions _______________________
 

; create-worm : ListOfPosn -> Worm
; Creates a worm from the given Posns, using the first Posn in the list
; as the worm's head, and the rest of the list, in that order, 
; as the worm's body.
; The resulting Worm may have other attributes of any value.
; WHERE: the list of posns are contiguous and form a valid worm
(begin-for-test
  (check-equal? (create-worm (list (make-posn 10 10)))
                (make-worm (make-posn 10 10) '() R)
                "create a worm with only head and empty body"))
; STRATEGY: function composition
(define (create-worm lop)
  (make-worm (first lop) (rest lop) R))



; create-worm : ListOfPosn Direction -> Worm
; Creates a worm from the given Posns, using the first Posn in the list
; as the worm's head, and the rest of the list, in that order, 
; as the worm's body.
; The resulting Worm use dir as its direction.
; WHERE: the list of posns are contiguous and form a valid worm
(begin-for-test
  (check-equal? (create-worm-dir (list (make-posn 10 10)) D)
                (make-worm (make-posn 10 10) '() D)
                "create a new worm from the give position (10, 10) and direction D"))
; STRATEGY: function composition
(define (create-worm-dir lop dir)
  (make-worm (first lop) (rest lop) dir))





; draw-posn : ListOfPosn -> Image
; Renders a list of dots
(begin-for-test
  (check-equal? (draw-posn '())
                MT
                "renders the empty scene"))
; STRATEGY: data decomposition on ListOfPosn : lst
(define (draw-posn lst)
  (cond
    [(empty? lst) MT]
    [else (place-image WORM-IMG
                       (posn-x (first lst))
                       (posn-y (first lst))
                       (draw-posn (rest lst)))]))






; rearrange-segment : ListOfPosn Direction -> ListOfPosn
; Rearrange the given list lst based on the given direction dir.
; This function will move the tail of the list before its own head 
; with regard to 4 different directions
(begin-for-test
  (check-equal? (rearrange-segment (make-posn 10 10)
                                   (list (make-posn 10 10))
                                   D
                                   FD)
                (append (list (new-posn (make-posn 10 10) D))
                        (eat-food->list (list (make-posn 10 10))
                                        (make-posn 10 10) 
                                        FD))
                "move the list's tail before its head"))
; STRATEGY: function composition
(define (rearrange-segment head lst dir fd)
  (append (list (new-posn head dir))
         (eat-food->list lst head fd)))






















; Worm Functions _________________________
 
; worm-length : Worm -> PosInt
; Returns the number of segments in the given worm.
(begin-for-test
  (check-equal? (worm-length INITIAL-WORM)
                1
                "the lenght of INITIAL-WORM is 1"))
; STRATEGY: data decomposition on Worm : wm
(define (worm-length wm)
  (+ 1 (length (worm-body wm))))
 


; worm-head-x : Worm -> Coordinate
; worm-head-y : Worm -> Coordinate
; Returns the x or y position of the center of the worm's lead segment.
(begin-for-test
  (check-equal? (worm-head-x INITIAL-WORM)
                RADIUS
                "the initial head x of the worm is 5")
  (check-equal? (worm-head-y INITIAL-WORM)
                RADIUS
                "the initial head y of the worm is 5"))
; STRATEGY: data decomposition on Worm : wm
(define (worm-head-x wm)
  (posn-x (worm-head wm))) 
(define (worm-head-y wm)
  (posn-y (worm-head wm))) 
 



; hit-wall? : Worm -> Boolean
; Returns true if the worm hit wall
(begin-for-test
  (check-equal? (hit-wall? (make-worm (make-posn 0 15) '() D))
                #true
                "hit left wall")
  (check-equal? (hit-wall? (make-worm (make-posn 300 15) '() D))
                #true
                "hit right wall")
  (check-equal? (hit-wall? (make-worm (make-posn 10 0) '() D))
                #true
                "hit up wall")
  (check-equal? (hit-wall? (make-worm (make-posn 10 1000) '() D))
                #true
                "hit down wall"))
; STRATEGY: function composition
(define (hit-wall? wm)
  (or (< (worm-head-x wm) LEFT-WALL)
      (> (worm-head-x wm) RIGHT-WALL)
      (< (worm-head-y wm) UP-WALL)
      (> (worm-head-y wm) DOWN-WALL)))



; hit-self? : Worm -> Boolean
; Returns true if the worm hit wall
(begin-for-test
  (check-equal? (hit-self? (make-worm (make-posn 5 15) (list (make-posn 5 15)) D))
                #true
                "hit itself"))
; STRATAGY: data decomposition on Worm : wm
(define (hit-self? wm)
  (posns-overlap? (worm-head wm) (worm-body wm)))

 



; turn-left : Worm -> Worm
; Return next worm when key 'left' was pressed
(begin-for-test
  (check-equal? (turn-left (make-worm (make-posn 10 15) '() R) FD)
                (make-worm (make-posn 10 15) '() R)
                "current worm is moving up, do nothing")
  (check-equal? (turn-left (make-worm (make-posn 10 15) '() U) FD)
                (worm-move (make-worm (make-posn 10 15) 
                                      '() L)
                           FD)
                "current worm is moving up, do nothing")
  (check-equal? (turn-left (make-worm (make-posn 10 15) '() L) FD)
                (worm-move (make-worm (make-posn 10 15) 
                                      '() L)
                           FD)
                "current worm is moving up, do nothing"))
; STRATAGY: data decomposition on Worm : wm
(define (turn-left wm fd)
  (cond 
    [(right? (worm-dir wm)) wm]
    [else (worm-move (make-worm (worm-head wm) (worm-body wm) L)
                     fd)]))




; turn-right : Worm -> Worm
; Return next worm when 'right' was pressed
(begin-for-test
  (check-equal? (turn-right (make-worm (make-posn 10 15) '() L) FD)
                (make-worm (make-posn 10 15) '() L)
                "current worm is moving up, do nothing")
  (check-equal? (turn-right (make-worm (make-posn 10 15) '() U) FD)
                (worm-move (make-worm (make-posn 10 15) 
                                      '() R)
                           FD)
                "current worm is moving up, do nothing")
  (check-equal? (turn-right (make-worm (make-posn 10 15) '() R) FD)
                (worm-move (make-worm (make-posn 10 15) 
                                      '() R)
                           FD)
                "current worm is moving up, do nothing"))
; STRATAGY: data decomposition on Worm : wm
(define (turn-right wm fd)
  (cond
    [(left? (worm-dir wm)) wm]
    [else (worm-move (make-worm (worm-head wm) (worm-body wm) R)
                     fd)]))




; turn-up : Worm -> Worm
; Return next worm when key 'up' was pressed
(begin-for-test
  (check-equal? (turn-up (make-worm (make-posn 10 15) '() D) FD)
                (make-worm (make-posn 10 15) '() D)
                "current worm is moving up, do nothing")
  (check-equal? (turn-up (make-worm (make-posn 10 15) '() U) FD)
                (worm-move (make-worm (make-posn 10 15) 
                                      '() U)
                           FD)
                "current worm is moving up, do nothing")
  (check-equal? (turn-up (make-worm (make-posn 10 15) '() U) FD)
                (worm-move (make-worm (make-posn 10 15) 
                                      '() U)
                           FD)
                "current worm is moving up, do nothing"))
; STRATAGY: data decomposition on Worm : wm
(define (turn-up wm fd)
  (cond
    [(down? (worm-dir wm)) wm]
    [else (worm-move (make-worm (worm-head wm) (worm-body wm) U)
                     fd)]))




; turn-down : Worm -> Worm
; Return next worm when key 'down' was pressed
(begin-for-test
  (check-equal? (turn-down (make-worm (make-posn 10 15) '() U) FD)
                (make-worm (make-posn 10 15) '() U)
                "current worm is moving up, do nothing")
  (check-equal? (turn-down (make-worm (make-posn 10 15) '() R) FD)
                (worm-move (make-worm (worm-head (make-worm (make-posn 10 15) '() R)) 
                                      (worm-body (make-worm (make-posn 10 15) '() R))
                                      D)
                           FD)
                "current worm is moving down, do nothing")
  (check-equal? (turn-down (make-worm (make-posn 10 15) '() L) FD)
                (worm-move (make-worm (worm-head (make-worm (make-posn 10 15) '() L)) 
                                      (worm-body (make-worm (make-posn 10 15) '() L))
                                      D)
                           FD)
                "current worm is moving down, do nothing"))
; STRATAGY: data decomposition on Worm : wm
(define (turn-down wm fd)
  (cond
    [(up? (worm-dir wm)) wm]
    ;[(down? (worm-dir wm)) wm]
    [else (worm-move (make-worm (worm-head wm) (worm-body wm) D)
                     fd)]))




; worm-move : Worm -> Worm
; Return next worm based on current worm state
(begin-for-test
  (check-equal? (worm-move INITIAL-WORM FD)
                (create-worm-dir (rearrange-segment (worm-head INITIAL-WORM)
                                  (worm->list INITIAL-WORM)
                                  (worm-dir INITIAL-WORM)
                                  FD)
                                 (worm-dir INITIAL-WORM))
                "first move of the initial world"))
; STRATAGY: data decomposition on Worm : wm
(define (worm-move wm fd)
  (create-worm-dir (rearrange-segment (worm-head wm)
                                  (worm->list wm)
                                  (worm-dir wm)
                                  fd)
                   (worm-dir wm)))




; worm->list : Worm -> ListOfPosn
; Constructs the worm wm to a list-of-posn
(begin-for-test
  (check-equal? (worm->list INITIAL-WORM)
                (list (worm-head INITIAL-WORM))
                "transfer the initial worm as a list of position"))
; STRATAGY: data decomposition on Worm : wm
(define (worm->list wm)
  (append (list (worm-head wm)) (worm-body wm)))




















; Direction Functions ———————————————————

; left? : Direction -> Boolean
; Returns true if the Direction d is L
(begin-for-test
  (check-true (left? L) "true"))
; STRATEGY: function composition
(define (left? d)
  (string=? d L))



; right? : Direction -> Boolean
; Returns true if the Direction d is R
(begin-for-test
  (check-true (right? R) "true"))
; STRATEGY: function composition
(define (right? d)
  (string=? d R))



; up? : Direction -> Boolean
; Returns true if the Direction d is U
(begin-for-test
  (check-true (up? U) "true"))
; STRATEGY: function composition
(define (up? d)
  (string=? d U))



; down? : Direction -> Boolean
; Returns true if the Direction d is D
(begin-for-test
  (check-true (down? D) "true"))
; STRATEGY: function composition
(define (down? d)
  (string=? d D))

























; Helper Functions _____________________




; new-posn : Posn Direction -> Posn
; Computes the new position based on the current position and direction
(begin-for-test
  (check-equal? (new-posn (make-posn 10 10) R)
                (make-posn 20 10)
                "moving right after one tick")
  (check-equal? (new-posn (make-posn 10 10) L)
                (make-posn 0 10)
                "moving left after one tick")
  
  (check-equal? (new-posn (make-posn 10 10) U)
                (make-posn 10 0)
                "moving up after one tick")
  (check-equal? (new-posn (make-posn 10 10) D)
                (make-posn 10 20)
                "moving down after one tick"))
; STRATAGY: data decomposition on Direction : dir
(define (new-posn head dir)
  (cond
    [(left? dir) (make-posn (- (posn-x head) DIAMETER) (posn-y head))]
    [(right? dir) (make-posn (+ (posn-x head) DIAMETER) (posn-y head))]
    [(up? dir) (make-posn (posn-x head) (- (posn-y head) DIAMETER))]
    [(down? dir) (make-posn (posn-x head) (+ (posn-y head) DIAMETER))]))





; ListOfPosn Posn Food -> ListOfPosn
; Removes the worm's tail if the current worm's head do not 
; overlap with the food, or else remains the worm's head and body unchanged.
(begin-for-test
  (check-equal? (eat-food->list '()
                                (make-posn 10 10)
                                (make-posn 10 10))
                '()
                "overlap, remain unchanged")
  (check-equal? (eat-food->list (list (make-posn 10 10))
                                (make-posn 100 100)
                                (make-posn 10 10))
                '()
                "do not overlap, remove tail of the list"))
; STRATEGY: function composition
(define (eat-food->list lst head food)
  (if (overlap? head food)
      lst
      (remove (first (reverse lst)) lst)))





; overlap? : Posn Posn -> Boolean
; Returns trun the give posn p1 p2 are overlapped
(begin-for-test
  (check-true (overlap? (make-posn 10 10) (make-posn 10 10))
              "overlapped"))
; STRATEGY: function composition
(define (overlap? p1 p2)
  (< (distance p1 p2) 
     DIAMETER))






; distance : Posn Posn -> NonNegReal
; Computes the distance between two posns
(begin-for-test
  (check-equal? (distance (make-posn 10 10) (make-posn 10 10))
                (sqrt (+ (sqr (- (posn-x (make-posn 10 10)) (posn-x (make-posn 10 10))))
                         (sqr (- (posn-y (make-posn 10 10)) (posn-y (make-posn 10 10))))))
                "distance between (make-posn 10 10) (make-posn 10 10) "))
; STRATAGY: data decomposition on Posn : p1 p2
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))






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
   (+ offset (* interval (random (quotient width interval))))
   (+ offset (* interval (random (quotient height interval))))))





; random-food : ListOfPosn -> Food
; Returns a randomly created posn as food
; WHERE: the created food do not overlay any part the worm
(begin-for-test
  (check-true (posn? (random-food (list (make-posn 10 10))))
              "check for random food"))
; STRATEGY: function composition
(define (random-food not-allowed)
  (food-check
   (random-posn WIDTH HEIGHT DIAMETER RADIUS)
  not-allowed))




; food-check : Food ListOfPosn -> Posn
; Return the food candidate-food if the candidate-food do not overlap
; any part the of given list not-allowed, or else recreate the candidate
; food again and recheck again
(begin-for-test
  (check-true (posn? (food-check (make-posn 10 10) (list (make-posn 10 10)))))
                "food overlap with list, random again")
; Strategy: generative recursion
(define (food-check candidate-food not-allowed)
  (if (posns-overlap? candidate-food not-allowed)
      (random-food not-allowed)
      candidate-food))




; posns-overlap? : Posn ListOfPosn -> Boolean
; Returns true if p overlaps with any elements of ps.
; Two posns touching at only their outer edges are not overlapping.
(begin-for-test
  (check-false (posns-overlap? (make-posn 10 10) (list (make-posn 100 100)))
               "do not overlap"))
; STRATAGY: data decomposition ListOfPosn : ps
(define (posns-overlap? p ps)
  (cond
    [(empty? ps) #false]
    [else (or (overlap? p (first ps))
              (posns-overlap? p (rest ps)))]))















; Main Function __________________________

(run INITIAL-WORLD TICK-RATE)















; ================= Alternate Data Definition =================



; 1. alternative data definition 1

; A ListOfPosns is either one of: 
; - (list Posn)
; - (cons Posn ListOfPosns)



; (define-struct worm [lst dir])
; A worm is a (make-worm ListOfPosns Direction)
; INTERP: lst represents the list of position of the worm, include
;         its head and body

; Other definitions Direction, Food, World remains the same.


; Pros:
;  - this definition will let some helper function disapper such as :

     ; (define (worm->list wm)
     ;   (append (list (worm-head wm)) (worm-body wm)))

;    and when it comes render the worm, we don't have to assmeble the worm
;    as a list any more.

;  - easier to compute next worm's positions as only one list.




; Cons:
;  - needs to decompose head from the worm's list in following functions:

     ;(define (new-posn head dir) ... )

     ;(define (eat-food->list lst head food) ... )
    
     ;(define (worm-head-x wm) ... )
  
     ;(define (worm-head-y wm) ... )

  
;    so need new helper functions













; 2. alternative data definition 2


; (define-struct world [worm food dir])
; A world is a (make-world Worm Food Direction)
; INTERP: worm represents the worm in the current world, food represents the 
;         food in the current world, dir represents the direction that the 
;         worm is now heading 

; other definition Worm, Food, Direction remains the same.


; Pros:
;  - easier to change the operate the direction of the worm, no need to decompose the worm 
;    any more. And less helper function and nested structure.

;(define (key-handler wd ke)
;  (cond
;    [(left? ke) (make-world (world-worm wd) (world-food wd) L)]
;    [(right? ke) (make-world (world-worm wd) (world-food wd) R)]
;    [(up? ke) (make-world (world-worm wd) (world-food wd) U)]
;    [(down? ke) (make-world (world-worm wd) (world-food wd) D)]
;    [else wd]))


; INSTEAD OF: 

;(define (key-handler wd ke)
;  (cond
;    [(left? ke) (make-world (turn-left (world-worm wd)) (world-food wd))]
;    [(right? ke) (make-world (turn-right (world-worm wd)) (world-food wd))]
;    [(up? ke) (make-world (turn-up (world-worm wd)) (world-food wd))]
;    [(down? ke) (make-world (turn-down (world-worm wd)) (world-food wd))]
;    [else wd]))


; Cons:
; - we have to use worm's direction to computes next worm's positions. So if 
;   we use this definition we have to pass direction from world to every layer
;   that need direction.






