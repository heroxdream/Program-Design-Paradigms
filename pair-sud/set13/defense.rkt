#lang racket

(require "extras.rkt")
(require rackunit)
(require lang/posn)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 20)

(provide Unit<%>)
(provide StatefulWorld<%>)
(provide mk-world)
(provide mk-ally)
(provide mk-enemy)
(provide mk-merc)

(define WIDTH 400)    ; pixels
(define HEIGHT 500)   ; pixels
(define MT (empty-scene WIDTH HEIGHT))
(define K 30)    ;  0 <= (random 30) < 10  Produce mercenary
(define K1 10)   ; 10 <= (random 30) < 20  Produce enemy
(define K2 20)   ; 20 <= (random 30) < 30  Produce ally
(define ZERO 0)  ; 
(define INITIAL-HEIGHT 50) ; pixels
(define RATE 5)            ; pixels/score
(define MAXVEL 5)          ; pixels/tick
(define MINVEL 1)          ; pixels/tick

; Represents one kind of behavior: Ally or Enemy
(define Behavior<%>
  (interface ()
    ; touch-border? : Posn Coordinate -> Boolean
    ; Returns true if the current obj touch the given border
    touch-border?
    
    ; within? : Posn Coordinate Coordinate -> Boolean
    ; Returns true
    within?
    
    ; get-img : -> Image
    ; Reutrns the Image of this behavior
    get-img
    
    ; get-color: -> String
    ; Reutrns the color of this behavior
    get-color))

; AllyBehavior% : A class that satisfies the Behavior<%> interface
; A AllyBehavior% is a (new AllyBehavior% )
; INTERP : Represents the behavior that ally have
(define AllyBehavior%
  (class* object% (Behavior<%>)
    (super-new)
    
    (define COLOR "green") ; color
    (define LENGTH 20)     ; square length
    (define IMG (rectangle LENGTH LENGTH "solid" COLOR)) ; image
    
    ; touch-border? : Posn Coordinate -> Boolean
    (define/public (touch-border? loc border)
      (>= (+ (posn-y loc) (/ LENGTH 2)) border))
    
    ; within? : Posn Coordinate Coordinate -> Boolean
    (define/public (within? loc x y)
      (and (< (- (posn-x loc) (/ LENGTH 2)) x (+ (posn-x loc) (/ LENGTH 2)))
           (< (- (posn-y loc) (/ LENGTH 2)) y (+ (posn-y loc) (/ LENGTH 2)))))
    
    ; get-img : -> Image
    (define/public (get-img)
      IMG)
    
    ; get-color: -> String
    (define/public (get-color)
      COLOR)))

; EnemyBehavior% : A class that satisfies the Behavior<%> interface
; A EnemyBehavior% is a (new EnemyBehavior% )
; INTERP : Represents the behavior that enemy have
(define EnemyBehavior%
  (class* object% (Behavior<%>)
    (super-new)
    
    (define COLOR "red")     ; color
    (define RADIUS 12)       ; radius of the circle
    (define IMG (circle RADIUS "solid" COLOR)) ; image of the circle
    
    ; touch-border? : Posn Coordinate -> Boolean
    (define/public (touch-border? loc border)
      (>= (+ (posn-y loc) (/ RADIUS 2)) border))
    
    ; within? : Posn Coordinate Coordinate -> Boolean
    (define/public (within? loc x y)
      (< (sqrt (+ (sqr (- x (posn-x loc))) (sqr (- y (posn-y loc))))) RADIUS))
    
    ; get-img : -> Image
    (define/public (get-img)
      IMG)
    
    ; get-color: -> String
    (define/public (get-color)
      COLOR)))

; Draw Unit<%>
(define Draw<%>
  (interface ()
    ; draw : Image -> Image
    ; Returns a image that represents the current object
    draw))

; Information of the Unit<%> in string format
(define ToString<%>
  (interface ()
    ; to-string : -> String
    ; Rerturns a string that contains information of the object
    to-string))

; Represents a unit in the game.
(define Unit<%>
  (interface ()
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn.
    get-loc
 
    ; get-color : -> Color
    ; Returns the color of this unit.
    get-color
    
    ; mutate! : -> Unit<%>
    ; EFFECT: Returns a new AllyUnit% or EnemyUnit% if the current Unit<%> is an
    ; instance of MercenaryUnit%
    mutate!
    
    ; move! : -> Void
    ; EFFECT: Moves a Unit<%> after one tick
    move!
    
    ; score-one-tick: Coordinate -> Number
    ; Computes the score after one tick if the Unit<%> is in base
    score-one-tick
    
    ; score-one-click: Coordinate Coordinate -> Number
    ; Computes the score after one click if the mouse is one the Unit<%>
    score-one-click))

; AllyUnit% : A class that satisfies the Unit<%> Draw<%> ToString<%> interface
; A AllyUnit% is a (new AllyUnit% [loc Posn] [v Number] [behavior Behavior<%>])
; INTERP : Represents a allyUnit with a loc, v and behavior
(define AllyUnit%
  (class* object% (Unit<%> Draw<%> ToString<%>)
    (super-new)
    (init-field loc ; location of the unit
                v   ; velocity of the unit in y coordinate
                [behavior (new AllyBehavior%)]) ; represents the behavior of 
                ; the current unit
    
    (define POINTS/POS 20) ; points get when eliminated
    (define POINTS/NEG -20) ; points lose when reach base
    
    ; get-loc : -> posn
    (define/public (get-loc) loc)
    
    ; get-color : -> Color
    (define/public (get-color) (send behavior get-color))
    
    ; mutate! : -> Void
    ; allys do not reponse to mutate!
    (define/public (mutate!) (void))
    
    ; move! : -> Void
    (define/public (move!)
      (set! loc (make-posn (posn-x loc) (+ v (posn-y loc)))))
    
    ; score-one-tick: Coordinate -> Number
    (define/public (score-one-tick border)
      (if (send behavior touch-border? loc border) POINTS/POS ZERO))
    
 
    ; draw : Image -> Image
    (define/public (draw bkg)
      (place-image (send behavior get-img) (posn-x loc) (posn-y loc) bkg))
    
    ; to-string : -> String
    (define/public (to-string)
      (string-append "AllyUnit location" (~v loc) 
                     " velocity" (~v v)
                     " | "))
    
    ; score-one-click: Coordinate Coordinate -> Number
    (define/public (score-one-click x y)
      (if (send behavior within? loc x y) POINTS/NEG ZERO))))

;EnemyUnit% : A class that satisfies the Unit<%> Draw<%> ToString<%> interface
;A EnemyUnit% is a (new EnemyUnit% [loc Posn] [v Number] [behavior Behavior<%>])
;INTERP : Represents a allyUnit with a loc, v and behavior
(define EnemyUnit%
  (class* object% (Unit<%> Draw<%> ToString<%>)
    (super-new)
    (init-field loc ; location of the enemyUnit
                v   ; velocity of the enemyUnit in y coordinate
                [behavior (new EnemyBehavior%)]) ; the behavior enemyUnit have
    
    (define POINTS/POS 40) ; points get when eliminated
    (define POINTS/NEG -40) ; points lose when reach base
    
    ; get-loc : -> posn
    (define/public (get-loc) loc)

    ; get-color : -> Color
    (define/public (get-color) (send behavior get-color))
    
    ; mutate! : -> Void
    (define/public (mutate!) (void))
    
    ; move! : -> Void
    (define/public (move!)
      (set! loc (make-posn (posn-x loc) (+ v (posn-y loc)))))
    
    ; score-one-tick: Coordinate -> Number
    (define/public (score-one-tick border)
      (if (send behavior touch-border? loc border) POINTS/NEG ZERO))
 
    ; draw : Image -> Image
    (define/public (draw bkg)
      (place-image (send behavior get-img) (posn-x loc) (posn-y loc) bkg))
    
    ; to-string : -> String
    (define/public (to-string)
      (string-append "EnemyUnit location" (~v loc)
                     " velocity" (~v v)
                     " | "))
    
    ; score-one-click: Coordinate Coordinate -> Number
    (define/public (score-one-click x y)
      (if (send behavior within? loc x y) POINTS/POS ZERO))))

; MercenaryUnit% : A class that satisfies the Unit<%> Draw<%> ToString<%> 
; interface
; A MercenaryUnit% is a (new MercenaryUnit% 
;                            [loc Posn] [v Number] [behavior Behavior<%>])
; INTERP : Represents a mercenaryUnit with a loc, v, behavior and ally
(define MercenaryUnit%
  (class* object% (Unit<%> Draw<%> ToString<%>)
    (super-new)
    (init-field loc ; location of the unit
                v   ; velocity of the unit in y coordinate
                [behavior (new AllyBehavior%)]) ; the behavior of the unit
    
    (define POINTS/POS 60) ; points get when eliminated
    (define POINTS/NEG -60) ; points lose when reach base
    
    ; get-loc : -> posn
    (define/public (get-loc) loc)

    ; get-color : -> Color
    (define/public (get-color) (send behavior get-color))
    
    ; mutate! : -> Void
    (define/public (mutate!) 
      (local((define new-behavior
               (if (is-a? behavior AllyBehavior%) 
                   (new EnemyBehavior%) 
                   (new AllyBehavior%))))
        (begin (set! behavior new-behavior))))
    
    ; move! : -> Void
    (define/public (move!)
      (set! loc (make-posn (posn-x loc) (+ v (posn-y loc)))))
    
    ; score-one-tick: Coordinate -> Number
    (define/public (score-one-tick border)
      (if (send behavior touch-border? loc border)
          (if (is-a? behavior AllyBehavior%) POINTS/POS POINTS/NEG) 
          ZERO))
    
    ; draw : Image -> Image
    (define/public (draw bkg)
      (place-image (send behavior get-img) (posn-x loc) (posn-y loc) bkg))
    
    ; to-string :  -> String
    (define/public (to-string)
      (string-append "MercenaryUnit location" (~v loc)
                     " velocity" (~v v)
                     " | "))
    
    ; score-one-click: Coordinate Coordinate -> Number
    (define/public (score-one-click x y)
      (if (send behavior within? loc x y) 
          (if (is-a? behavior AllyBehavior%) POINTS/NEG POINTS/POS) 
          ZERO))))
 
; Represents a mutable world object.
(define StatefulWorld<%>
  (interface ()
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    on-tick!
 
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the given mouse
    ; parameters.
    on-mouse!
 
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    target-loc
 
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    get-units
 
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    add-unit!
    
    ; after-tick : -> StatefulWorld<%>
    ; EFFECT: Returns the world after one tick
    after-tick!
    
    ; after-click : -> StatefulWorld<%>
    ; EFFECT: Returns the world after one click
    after-click!))

; World% : A class that satisfies the StatefulWorld<%> Draw<%> ToString<%> 
; interface
; A World% is a (new World% 
;                   [units ListOf<Unit<%>>] 
;                   [minvel Number] 
;                   [maxvel Number]
;                   [tx Number] 
;                   [ty Number] 
;                   [tick Number] 
;                   [score Number] )
; INTERP : Represents a World% with a units, minvel, maxvel tx, ty, tick and 
;          score
(define World%
  (class* object%(StatefulWorld<%> Draw<%> ToString<%>)
    (super-new)
    (init-field units     ; a list of Unit<%> of the world
                minvel    ; min velocity of the unit
                maxvel    ; max velocity of the unit
                [tx 0]    ; the current x-coordinate of the target ( or mouse )
                [ty 0]    ; the current y-coordinate of the target ( or mouse )
                [tick 0]  ; a counter that records how many ticks have pasted
                [score 0]); the score of the world

    (define RATE/ADD 4)    ; /ticks
    (define RATE/MUTATE 3) ; /ticks
    (define GAMEOVER-IMG (text "GAME OVER" 40 "red")) ; text represent game over
    (define MAX-SCORE 2250) ; max score
    (define MIN-SCORE -200) ; min score
    (define OUTTER-RADIUS 10) ; outter radius of the target
    (define INNER-RADIUS 5)   ; inner radius of the target
    (define TARGET-IMG        ; the image of the target
      (scene+line 
       (scene+line 
        (place-image  (circle INNER-RADIUS "outline" "red") 
                      OUTTER-RADIUS OUTTER-RADIUS
                      (circle OUTTER-RADIUS "outline" "red")) 
                  ZERO OUTTER-RADIUS (* 2 OUTTER-RADIUS) OUTTER-RADIUS "black") 
       OUTTER-RADIUS ZERO OUTTER-RADIUS (* 2 OUTTER-RADIUS) "black"))
    
    ; on-tick! : -> Void
    (define/public (on-tick!)
      (set! tick (add1 tick)) ; tick ++
      (unit-move!)            ; all units move by one tick
      (define remain/units (remain-one-tick));filter remain units after one tick
      (define score/new (+ score (score-increment-after-one-tick)));new score
      (set! units remain/units) ; updates units
      (set! score score/new)    ; updates score
      (if (= 0 (modulo tick RATE/ADD))  ; (tick%4 == 0) add new random unit
          (add-unit! (random-new-unit maxvel minvel))
          (void tick))
      (if (= 0 (modulo tick RATE/MUTATE)) ; (tick%3 == 0)mutate mercenary units.
          (unit-mutate!) 
          (void tick)))
    
    ; score-increment-after-one-tick: -> Number
    ; Computes the accumulated change of the score of all units.
    (define (score-increment-after-one-tick)
      (foldr (lambda (u s) (+ s (send u score-one-tick (border)))) ZERO units))
    
    ; unit-move!: -> Void
    ; EFFECT: Moves each unit in units after one tick
    (define (unit-move!)
      (for-each (lambda (u) (send u move!)) units))
    
    ; unit-mutate! -> Void
    ; EFFECT: change the behavior of each unit if
    (define (unit-mutate!)
      (for-each (lambda (u) (send u mutate!)) units))
    
    ; remain-one-tick : -> ListOf<Unit<%>>
    ; Returns the Unit<%>s that still exists in the world after one tick
    (define (remain-one-tick)
      (filter (lambda (u) (= ZERO (send u score-one-tick (border)))) units))
    
    ; border : -> Number
    ; Returns the y-coordinate of the border of the base.
    (define (border) (- HEIGHT (get-base-height)))
 
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; Strategy: data decomposition on e : MouseEvent
    (define/public (on-mouse! x y e)
      (cond
        [(string=? e "button-down") 
         (begin
           (set! score (+ score (score-increment-one-click x y))) ; update score
           (set! units (remain-units-one-click x y)))] ; updates units
        [else (begin (set! tx x) (set! ty y))])) ; updates target's x y coor
    
    ; score-increment-one-click -> Number
    ; Computes the accumulated change of the score of all units after on click. 
    (define (score-increment-one-click x y)
      (foldr (lambda (u s) (+ s (send u score-one-click x y))) 0 units))
    
    ; remain-units-one-click : -> ListOf<Unit<%>>
    ; Returns the Unit<%>s that still exists in the world after one click
    (define (remain-units-one-click x y)
      (filter (lambda (u) (= ZERO (send u score-one-click x y))) units))
 
    ; target-loc : -> posn
    (define/public (target-loc) (make-posn tx ty))
 
    ; get-units : -> ListOf<Unit<%>>
    (define/public (get-units) units)
 
    ; add-unit! : Unit<%> -> Void
    (define/public (add-unit! u) (set! units (cons u units)))
 
    ; get-base-height : -> Natural
    ; Returns the height of the base, in pixels.
    (define (get-base-height) (+ INITIAL-HEIGHT (/ score RATE)))
    
    ; draw-units-base-score: Imgae -> Image
    ; draw the units base and score of the world
    (define (draw-units-base-score bkg)
      (local((define units-base-img 
               (foldr (lambda (u img) (send u draw img)) 
                      (place-image (base-img) (base-x) (base-y) bkg) 
                      units)))
        (if (game-over?)
            (overlay/align "middle" "bottom" GAMEOVER-IMG units-base-img)
            (overlay/align "middle" "bottom" (score-img) units-base-img))))
    
    ; draw : Image -> Image
    (define/public (draw bkg)
      (place-image TARGET-IMG tx ty (draw-units-base-score bkg)))
    
    ; after-tick! : -> StatefulWorld<%>
    (define/public (after-tick!)
      (if (game-over?) this (begin (on-tick!) this)))
    
    ; game-over? : -> Boolean
    ; Returns true if the world game is over
    (define (game-over?)
      (or (<= score MIN-SCORE) (>= score MAX-SCORE)))
    
    ; after-click! : -> StatefulWorld<%>
    (define/public (after-click! x y e)
      (if (game-over?) this (begin (on-mouse! x y e) this)))

    ; base-img : -> Image
    ; Returns the image represents the base
    (define (base-img)
      (rectangle WIDTH (get-base-height) "solid" "yellow"))
    
    ; score-img : -> Image
    ; Returns the image represents the score.
    (define (score-img) 
      (text (string-append "Score: " (number->string score)) 40 "black"))
    
    ; base-x : -> Coordinate
    ; Returns the x-coordinate of the base
    (define (base-x) (/ WIDTH 2))
    
    ; base-y : -> Coordinate
    ; Returns the y-coordinate of the base
    (define (base-y) (- HEIGHT (/ (get-base-height) 2)))
    
    ; to-string : -> String
    (define/public (to-string)
      (local((define units-str 
               (foldr (lambda (u str) 
                        (string-append (send u to-string) str)) "" units)))
        (string-append units-str 
                       " | score "  (~v score)
                       " | height " (~v (get-base-height))
                       " | border " (~v (border))
                       " | minvel "  (~v minvel)
                       " | maxvel "  (~v maxvel)
                       " | tick "  (~v tick))))))

; A Velocity is a Natural, representing Pixels/tick in the downward direction.
 
; mk-world : Velocity Velocity Natural -> StatefulWorld<%>
; Creates a world with num-units initial random units,
; where units have the specified min and max velocity.
; WHERE: minvel <= maxvel
(define (mk-world maxvel minvel num-units) 
  (new World% [units (rand-units maxvel minvel num-units)]
       [minvel minvel] [maxvel maxvel]))

; rand-units : Number Number Number -> ListOf<Unit<%>>
; Returns a list randomed Unit<%>s whose velocity is between mxvel and minvel
; Strategy: function composition
(define (rand-units maxvel minvel num-units)
  (local ((define lst '()))
    (for ([i num-units])
      (set! lst (cons (random-new-unit maxvel minvel) lst)))
    lst))

; random-new-unit : Number Number -> Unit<%>
; Returns a new Unit<%> whose velocity is between maxvel and minvel
; Strategy: function composition
(define (random-new-unit maxvel minvel)
  (if (<= K2 (random K) K)
      (mk-ally (make-posn (random WIDTH) ZERO) (rand/vel maxvel minvel))
      (if (<= K1 (random K) K2)
          (mk-enemy (make-posn (random WIDTH) ZERO) (rand/vel maxvel minvel))
          (mk-merc (make-posn (random WIDTH) ZERO) (rand/vel maxvel minvel)))))

; rand/vel : Number Number -> Number
; Returns a randomed number between maxvel and minvel
; Strategy: function composition
(define (rand/vel maxvel minvel)
  ( + minvel (ceiling (* (- maxvel minvel) (random)))))

; mk-enemy : posn Velocity -> Unit<%>
; Creates an enemy unit with the given parameters.
; Strategy: function compositon
(define (mk-enemy po v)
  (new EnemyUnit% [loc po] [v v]))
 
; mk-ally : posn Velocity -> Unit<%>
; Creates an ally unit with the given parameters.
; Strategy: function compositon
(define (mk-ally po v)
  (new AllyUnit% [loc po] [v v]))
 
; mk-merc : posn Velocity -> Unit<%>
; Creates a mercenary unit with the given parameters.
; Strategy: function compositon
(define (mk-merc po v)
  (new MercenaryUnit% [loc po] [v v]))

(define WORLD (mk-world MAXVEL MINVEL 10))

; run-world : World<%> -> World<%>
; Computes next World<%> based on the current World<%>
; Strategy: function composition
(define (run-world initial-world)
  (big-bang initial-world
            (on-tick (λ (w) (send w after-tick!)))
            (on-mouse (λ (w x y mev) (send w after-click! x y mev)))
            (on-draw (λ (w) (send w draw MT)))))

; run : -> World%
; Computes next World<%> based on the current World<%>
; Strategy: function compositon
(define (run) (run-world WORLD))

; Code cover Test:

(define WORLD2 (mk-world 400 1 500))

(define A (new AllyUnit% [loc (make-posn 200 449)] [v 10]))
(define E (new EnemyUnit% [loc (make-posn 200 449)] [v 10]))
(define M (new MercenaryUnit% [loc (make-posn 200 449)] [v 10]))

(define A2 (new AllyUnit% [loc (make-posn 200 1000)] [v 10]))
(define E2 (new EnemyUnit% [loc (make-posn 200 1000)] [v 10]))
(define M2 (new MercenaryUnit% [loc (make-posn 200 1000)] [v 10]))

(define M3 (new MercenaryUnit% 
                [loc (make-posn 200 1000)] 
                [v 10] 
                [behavior (new EnemyBehavior%)]))

(define M4 (new MercenaryUnit% 
                [loc (make-posn 200 499)] 
                [v 10] 
                [behavior (new EnemyBehavior%)]))

(define WORLD3 (new World% 
                    [units (list A E M A2 E2 M2 M3 M4)]
                    [minvel 1]
                    [maxvel 10]
                    [tx 200]
                    [ty 499]
                    [tick 2]
                    [score 200]))

(define WORLD4 (new World% 
                    [units (list A E M A2 E2 M2 M3 M4)]
                    [minvel 1]
                    [maxvel 10]
                    [tx 200]
                    [ty 499]
                    [tick 3]
                    [score -200]))

(define WORLD5 (new World% 
                    [units (list A E M A2 E2 M2 M3 M4)]
                    [minvel 1]
                    [maxvel 10]
                    [tx 200]
                    [ty 499]
                    [tick 4]
                    [score -200]))

(define WORLD6 (new World% 
                    [units (list A)]
                    [minvel 1]
                    [maxvel 10]
                    [tx 200]
                    [ty 499]
                    [tick 4]
                    [score -200]))

(begin-for-test
  (check-true (image? (send WORLD draw MT)))
  (check-true (image? (send WORLD2 draw MT)))
  (check-true (image? (send WORLD3 draw MT)))
  (check-true (image? (send WORLD4 draw MT)))
  (check-true (image? (send WORLD5 draw MT)))
  (check-true (object? (send WORLD3 after-tick!)))
  (check-true (object? (send WORLD3 after-tick!)))
  (check-true (object? (send WORLD4 after-tick!)))
  (check-true (object? (send WORLD5 after-tick!)))
  (check-true (object? (send WORLD2 after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD after-tick!)))
  (check-true (object? (send WORLD2 after-click! 200 200 "button-down")))
  (check-true (object? (send WORLD after-click! 200 200 "button-down")))
  (check-true (object? (send WORLD after-click! 200 200 "button-up")))
  (check-true (object? (send WORLD3 after-click! 200 509 "button-down")))
  (check-true (object? (send WORLD4 after-click! 200 509 "button-down")))
  (check-true (object? (send WORLD5 after-click! 200 509 "button-down")))
  (check-true (string? (send WORLD2 to-string)))
  (check-equal? (length (send WORLD get-units)) 12)
  (check-equal? (send M4 score-one-click 200 509)
                60)
  (check-equal? (send A get-color)
                "green")
  (check-equal? (send E get-color)
                "red")
  (check-equal? (send M get-color)
                "green")
  (check-equal? (send M3 get-color)
                "red")
  (check-equal? (send A get-loc)
                (make-posn 200 459))
  (check-equal? (send E get-loc)
                (make-posn 200 459))
  (check-equal? (send M get-loc)
                (make-posn 200 459))
  (check-equal? (send WORLD2 target-loc)
                (make-posn 0 0))
  (check-equal? (send WORLD6 get-units)
                (list A))
  (check-equal? (send A score-one-click 200 459)
                -20)
  (check-equal? (send E score-one-click 200 459)
                40)
  (check-equal? (send M2 score-one-click 200 1010)
                -60))

;Alternate Designs:
; - part1:
; In this assignment we can use publish-subscriber pattern. For example:
; Unit<%> can be publishers and World% can be subscriber. Every time a unit
; clicked by the target or it touch the base's border, the unit can publish 
; this situation to World, and change score and units in World. we did not use
; this pattern beacuse Unit will have the authoritive to change world's socre.
; And unit will have the authoritive to remove or add itself from the units 
; list of the World, which is unsafe for World class. After all, units in this 
; game just exists few seconds, in another word, they are transient. And more 
; importantly, there are lots of units generated and died in this game possibly 
; at anytime. We can not give units the authority to change the property of the 
; world since it's unsafe for the world state.

; - part2:
; In the design of Unit<%>, we could have not abstract the Behavior<%> 
; interface and just wrote everything in each Unit class. We did not do in that
; way because it will be nasty to write Mercenary. Mercenary will mutate between
; Ally and Enemy every 3 second. If we write condition clause in the Mercenary 
; class, it will be very unclearn. But with the abstraction of Behavior<%> we 
; eliminated all those cond clause.
; By the way, the abstraction of Behavior some what like Visitor Pattern