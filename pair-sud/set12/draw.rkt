#lang racket
(require "extras.rkt")
(require "triangle.rkt")
(require rackunit)
(require lang/posn)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 15)

(provide INITIAL-WORLD)
(provide handle-mouse)
(provide Shape<%>)
(provide get-world-shapes)
(provide create-rectangle)
(provide create-circle)

; constants
(define MT (empty-scene 600 400))
(define BLACK-SQUARE (rectangle 20 20 "solid" "black"))
(define WHITE-SQUARE (rectangle 20 20 "outline" "black"))
(define WHITE-P (text "p" 16 "white"))
(define BLACK-P (text "p" 16 "black"))
(define WHITE-R (text "r" 16 "white"))
(define BLACK-R (text "r" 16 "black"))
(define WHITE-C (text "c" 16 "white"))
(define BLACK-C (text "c" 16 "black"))
(define WHITE-T (text "t" 16 "white"))
(define BLACK-T (text "t" 16 "black"))
(define POSN (make-posn 0 0))
(define VS (make-posn 0 0))
(define TOOLBAR-LEFT 0)
(define TOOLBAR-RIGHT 20)
(define TOOLBAR-TOP 0)
(define TOOLBAR-BOTTOM 80)
(define BETWEEN/P/R 20)
(define BETWEEN/R/C 40)
(define BETWEEN/C/T 60)

; A BoundingBox is a (list Coordinate Coordinate Coordinate Coordinate)
; INTERPRETATION: (list left top right bottom).
; A BoundingBox represents a box whose left x-coordinate is at "left", whose
; top y-coordinate is at "top", whose right x-coordinate is at "right", and 
; whose bottom y-coordinate is at "bottom".
; Template:
; boundingBox-fn : BoundingBox -> ???
; Strategy: data decomposition on bb: BoundingBox
;(define (boundingBox-fn bb)
;  (...(first bb)...(second bb)...(third bb)...(fourth bb)))

; A ShapeState is one of:
; - created
; - creating
; - moving
; - resizing
; Represents the current state of the shape
(define created "created")
(define creating "creating")
(define moving "moving")
(define resizing "resizing")
; Template: 
; shapeState-fn: ShapeState -> ???
; Strategy: data decomposition on ss: ShapeState
;(define (shapeState-fn ss)
;  (cond
;    [(created? ss) ...]
;    [(creating? ss) ...]
;    [(moving? ss) ...]
;    [(resizing? ss) ...]))

; created?: ShapeState -> Boolean
; creating?: ShapeState -> Boolean
; moving?: ShapeState -> Boolean
; resizing?: ShapeState -> Boolean
; Returns true if the current state is created/creating/moving/resizing.
; Strategy: Function composition
(begin-for-test
  (check-true (created? created))
  (check-true (creating? creating))
  (check-true (moving? moving))
  (check-true (resizing? resizing)))
(define (created? state)
  (string=? state created))
(define (creating? state)
  (string=? state creating))
(define (moving? state)
  (string=? state moving))
(define (resizing? state)
  (string=? state resizing))

; A DrawState is one of:
; - pointer
; - rectangle
; - circle
; - triangle
(define pointer "pointer")
(define rec "rectangle")
(define cir "circle")
(define tri "triangle")
; Template:
; drawState-fn -> ???
; Strategy: data decomposition on ds: DrawState
;(define (drawState-fn ds)
;  (cond
;    [(pointer? ds) ...]
;    [(rectangle? ds) ...]
;    [(circle? ds) ...]
;    [(triangle? ds) ...]))

; pointer? : String -> Boolean
; Returns true if string matches
; Strategy: Function composition
(begin-for-test
  (check-equal? (pointer? "pointer") #true
                "Test failed. Function should return true"))
(define (pointer? state)
  (string=? pointer state))

; rectangle? : String -> Boolean
; Returns true if string matches
; Strategy: Function composition
(begin-for-test
  (check-equal? (rectangle? "rectangle") #true
                "Test failed. Function should return true"))
(define (rectangle? state)
  (string=? rec state))

; triangle? : String -> Boolean
; Returns true if string matches
; Strategy: Function composition
(begin-for-test
  (check-equal? (triangle? "triangle") #true
                "Test failed. Function should return true"))
(define (triangle? state)
  (string=? tri state))

; circle? : String -> Boolean
; Returns true if string matches
; Strategy: Function composition
(begin-for-test
  (check-equal? (circle? "circle") #true
                "Test failed. Function should return true"))
(define (circle? state)
  (string=? cir state))

(define Shape<%>
  (interface ()
    ; get-bounds : -> BoundingBox
    ; Returns the BoundingBox of this shape
    get-bounds
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Shape<%>
    ; Returns a new shape after handling MouseEvent on this shape
    handle-mouse))

(define Drawable<%>
  (interface ()
    ; draw -> Image
    ; Return a Image which is the image of the shape
    draw))

(define Comparable<%>
  (interface ()
    ; equal-to? : Shape<%> -> Bolean
    ; Returns true if the current shape is equal to the target shape
    equal-to?
    
    ; hash-code: -> Number
    ; Returns the hashCode of the Shape;This HashCode is attained by 
    ; multiplying big prime numbers to each elements of the shape, then 
    ; add them up.
    hash-code))

(define ToString<%>
  (interface ()
    ; test:to-string : -> String
    ; Convert this Shape to a String format, which presents the enssential 
    ; information of the shape.
    test:to-string))

; Rectangle% : A class that satisfies the Drawable<%> Comparable<%> ToString<%>
;             interface
; A Rectangle% is a (new Rectangle% [stable Posn] [dynamic Posn] 
;                    [state ShapeState] [v/s Posn] [v/c Posn])
; INTERP : Represents a rectangle, with a stable and dynamic point, state,
;          vector start point and vector center point.
(define Rectangle%
  (class* object% (Shape<%> Drawable<%> Comparable<%> ToString<%>)
    
    (init-field stable dynamic state v/s v/c)
    ; INTERP 'stable' is the posn for stable point of the shape,
    ; 'dynamic' is the posn for the dynamic point of the shape which changes 
    ; during the drag event, 
    ; 'state' is a ShapeState that represents the current state of the shape
    ; 'v/s' is a posn for vector for moving the shape. 
    ; 'v/c' is a posn that records the initial point of the rectangle's center.
    
    (super-new)
    
    ; limitation of control boundary
    (define BNDY 5) 
    
    ; get-bounds : -> BoundingBox
    ; Returns a list of coordinate representing a rectangle
    ; Startegy: Data decomposition on stable,dynamic : Posn
    (define/public (get-bounds) (list (min (posn-x stable) (posn-x dynamic))
                                      (min (posn-y stable) (posn-y dynamic))
                                      (max (posn-x stable) (posn-x dynamic))
                                      (max (posn-y stable) (posn-y dynamic))))
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Rectangle%
    ; Returns the object of the class depending upon the mouse event
    ; Startegy: Data decomposition on e : MouseEvent
    (define/public (handle-mouse cx cy e) 
      (cond 
        [(string=? e "button-down") (gesture-start cx cy)]
        [(string=? e "drag") (gesturing cx cy)]
        [(string=? e "button-up") (gesture-end cx cy)]))
    
    ; gesture-start : Coordinate Coordinate -> Rectangle%
    ; Returns a new rectangle by checking the state of the object
    ; Strategy: Data decomposition on state: ShapeState
    (define (gesture-start cx cy)
      (cond
        [(or (moving? state) (resizing? state) (creating? state)) 
         (build/r (make-posn cx cy) (make-posn cx cy) state v/s v/c)]
        [(created? state) (init/resize/move cx cy)]))
    
    ; init/resize/move : Coordinate Coordinate -> Rectangle%
    ; Init resize sate or move state a rectangle depending on the current state 
    ; of object
    (define (init/resize/move cx cy)
      (local((define stable (control-posn cx cy)))
        (if (posn? stable)
            (init-resize stable cx cy)
            (if (inside/this? cx cy)
                (init-move cx cy)
                this))))
    
    ; init-resize : Posn Coordinate Coordinate -> Rectangle% 
    ; Returns a new initialized resize state rectangle by changing the dynamic 
    ; point of rectangle
    (define (init-resize stable cx cy)
      (build/r stable (make-posn cx cy) resizing v/s v/c))
    
    ; init-move : Posn Coordinate Coordinate -> Rectangle% 
    ; Returns a new initialized move state rectangle by changing the vector 
    ; position
    (define (init-move cx cy)
      (build/r stable dynamic moving (make-posn cx cy) 
               (make-posn (get-centerX) (get-centerY))))
    
    ; gesturing : Coordinate Coordinate -> Rectangle%
    ; Returns a new rectangle based on the state of the rectangle when draging
    ; the mouse
    ; Strategy: Data decomposition on state: ShapeState
    (define (gesturing cx cy)
      (cond
        [(or (resizing? state) (creating? state))
         (build/r stable (make-posn cx cy) state v/s v/c)]
        [(moving? state) (add/vector cx cy)]
        [(created? state) this]))
    
    ; add/vector : Coordinate Coordinate -> Rectangle%
    ; Returns a new rectangle with stable and dynamic positions updated
    ; according to the coordinates passed and initial posn.
    ; Startegy: Data decomposition on v/s, v/c, stable, dynamic : Posn
    (define (add/vector cx cy)
      (local((define vx (- cx (posn-x v/s)))
             (define vy (- cy (posn-y v/s)))
             (define new/center (make-posn (+ (posn-x v/c) vx)
                                           (+ (posn-y v/c) vy)))
             (define dis/x (/ (abs (- (posn-x stable) (posn-x dynamic))) 2))
             (define dis/y (/ (abs (- (posn-y stable) (posn-y dynamic))) 2))
             (define new/sta (make-posn (- (posn-x new/center) dis/x) 
                                        (- (posn-y new/center) dis/y)))
             (define new/dyn (make-posn (+ (posn-x new/center) dis/x) 
                                        (+ (posn-y new/center) dis/y))))
        (build/r new/sta new/dyn state v/s v/c)))
    
    ; gesture-end : Coordinate Coordinate -> Rectangle%
    ; Returns a new rectangle with state as created
    (define (gesture-end cx cy)
      (build/r stable dynamic created v/s v/c))
    
    ; control-posn : Coordinate Coordinate -> Maybe<Posn>
    ; Returns a the stable posn by the given click point, if the click point is
    ; within the control regin of any coners of the rectanle.
    ; Example: lets assume the four coners of the rectangle are (c1 c2 c3 c4) 
    ; in clockwise order. If the click point is within control region of c1, 
    ; return c3, if c2, returns c4 and so on.
    (define (control-posn cx cy)
      (local((define cnr1 (make-posn (posn-x stable) (posn-y dynamic)))
             (define cnr2 (make-posn (posn-x dynamic) (posn-y stable)))
             (define coners (list stable dynamic cnr1 cnr2))
             (define hmap (make-hash (list (list cnr1 cnr2) (list cnr2 cnr1)
                                           (list stable dynamic)
                                           (list dynamic stable))))
             (define ctr-c 
               (filter (lambda (c) (inside? c cx cy BNDY BNDY)) coners)))
        (if (empty? ctr-c) #false (first (hash-ref hmap (first ctr-c))))))
    
    ; inside/this? : Coordinate Coordinate -> Boolean
    ; Returns true if the mouse click is inside the rectangle
    ; Strategy: Data decomposition on stable,dynamic : Posn
    (define (inside/this? cx cy)
      (inside? (make-posn (get-centerX) (get-centerY))
               cx cy 
               (/ (abs (- (posn-x stable) (posn-x dynamic))) 2)
               (/ (abs (- (posn-y stable) (posn-y dynamic))) 2)))
    
    ; draw : -> Image
    ; Returns an image of the rectangle
    ; Strategy: Data decomposition on stable,dynamic : Posn
    (define/public (draw background) 
      (cond
        [(string=? creating state) 
         (place-image (rectangle (abs (- (posn-x stable) (posn-x dynamic))) 
                                 (abs (- (posn-y stable) (posn-y dynamic)))
                                 127 "red")
                      (get-centerX) (get-centerY) background)]
        [else 
         (place-image (rectangle (abs (- (posn-x stable) (posn-x dynamic))) 
                         (abs (- (posn-y stable) (posn-y dynamic)))
                         "outline" "black")
                      (get-centerX) (get-centerY) background)]))
    
    ; get-centerX : -> Coordinate
    ; Returns the x coordinate of the center of the rectangle
    ; Strategy: Data decomposition on stable,dynamic : Posn
    (define (get-centerX) 
      (/ (+ (posn-x stable) (posn-x dynamic)) 2))
    
    ; get-centerY : -> Coordinate
    ; Returns the y coordinate of the center of the rectangle
    ; Strategy: Data decomposition on stable,dynamic : Posn
    (define (get-centerY) 
      (/ (+ (posn-y stable) (posn-y dynamic)) 2))
    
    ; test:to-string: Rectangle% -> Boolean
    (define/public (equal-to? other)
      (and (is-a? other Rectangle%)
           (= (hash-code) (send other hash-code))))
    
    ; test:to-string: -> Number
    (define/public (hash-code)
      (+(* 4398042316799 (posn-x stable))
        (* 16769023 (posn-x dynamic))
        (* 274876858367 (posn-y stable))
        (* 68718952447 (posn-y dynamic))
        (hash/shape-state state)))
    
    ; test:to-string: -> String
    (define/public (test:to-string)
      (string-append "stable" (~v stable) 
                     "| dynamic" (~v dynamic) 
                     "| state: " state 
                     "| v/s"(~v v/s) 
                     "| v/c"(~v v/c)))))

; Circle% : A class that satisfies the Shape<%> Drawable<%> Comparable<%>
;          ToString<%> interface
; A circle is a (new Rectangle [center Posn] [radius PosInt] 
;                   [state StateShape] [v/s Posn] [v/c Posn])
; INTERP : Represents a circle with a center, radius, state
; and vector start point, initial center point
(define Circle%
  (class* object% (Shape<%> Drawable<%> Comparable<%> ToString<%>)
    (init-field center radius state v/s v/c)
    ; INTERPRETATION: 'center' is the posn for center of the circle,
    ; 'radius' is the positive integer for the circle which changes 
    ; during the drag event, 
    ; 'state' is a ShapeState that represents the current state of the shape
    ; 'v/s' is a posn for vector for moving the shape
    ; 'v/c' is a posn represents the initial center point of the circle.  
    
    (super-new)
    
    ; the limitation of the control region boundary
    (define BNDY 2)
    
    ; get-bounds : -> BoundingBox
    ; Returns a list of coordinate representing a circle
    ; Startegy: Data decomposition on center : Posn
    (define/public (get-bounds) (list (- (posn-x center) radius)
                                      (- (posn-y center) radius)
                                      (+ (posn-x center) radius)
                                      (+ (posn-y center) radius)))
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Circle%
    ; Returns a new object of Circle% depending upon the mouse event
    ; Startegy: Data decomposition on e : MouseEvent
    (define/public (handle-mouse cx cy e) 
      (cond
        [(string=? e "button-down") (gesture-start cx cy)]
        [(string=? e "drag") (gesturing cx cy)]
        [(string=? e "button-up") (gesture-end cx cy)]))
    
    ; gesture-start : Coordinate Coordinate -> Circle%
    ; Returns a new circle  by checking the state of the object
    ; Strategy: Data decomposition on state: ShapeState
    (define (gesture-start cx cy)
      (cond
        [(or (moving? state) (resizing? state) (creating? state)) 
         (build/c (make-posn cx cy) 0 state v/s v/c)]
        [(created? state) (init/resize/move cx cy)]))
    
    ; init/resize/move : Coordinate Coordinate -> Circle%
    ; Init resizing or moving a new circle depending on the current state of 
    ; object
    (define (init/resize/move cx cy)
      (local((define controlable (controlable? cx cy)))
        (if controlable
            (init-resize cx cy)
            (if (inside/this? cx cy)
                (init-move cx cy)
                this))))
    
    ; controlable? : Coordinate Coordinate -> Boolean
    ; Returns true if the mouse click is within the control boundary
    (define (controlable? cx cy)
      (<= (- radius BNDY) (radius-now cx cy) (+ radius BNDY)))
    
    ; inside/this? : Coordinate Coordinate -> Boolean
    ; Returns true if the mouse click is inside the circle
    (define (inside/this? cx cy)
      (< (radius-now cx cy) radius))
    
    ; init-resize : Coordinate Coordinate -> Circle% 
    ; Returns a new resized circle by changing the radius
    (define (init-resize cx cy)
      (build/c center (radius-now cx cy) resizing v/s v/c))
    
    ; init-move : Coordinate Coordinate -> Circle% 
    ; Returns a new circle by changing the vector position
    (define (init-move cx cy)
      (build/c center radius moving (make-posn cx cy) center))
    
    ; gesturing : Coordinate Coordinate -> Circle%
    ; Returns a new circle based on the state of the circle
    ; Strategy: Data decomposition on state: ShapeState
    (define (gesturing cx cy)
      (cond
        [(or (resizing? state) (creating? state)) 
         (build/c center (radius-now cx cy) state v/s v/c)]
        [(moving? state) (add/vector cx cy)]
        [(created? state) this]))
    
    ; gesture-end : Coordinate Coordinate -> Circle%
    ; Returns a new circle with state as created
    (define (gesture-end cx cy)
      (build/c center radius created v/s v/c))
    
    ; add/vector : Coordinate Coordinate -> Circle%
    ; Returns a new cirlce with center updated according to the 
    ; coordinates passed and initial center posn.
    ; Startegy: Data decomposition on v/c: Posn
    (define (add/vector cx cy)
      (local((define vx (- cx (posn-x v/s)))
             (define vy (- cy (posn-y v/s)))
             (define new/center (make-posn (+ (posn-x v/c) vx)
                                           (+ (posn-y v/c) vy))))
        (build/c new/center radius state v/s v/c)))
    
    ; radius-now : Coordinate Coordinate -> PosInt
    ; Returns the radius of circle by calculating the value from the center
    ; and the current mouse position
    ; Strategy: Data decomposition on center : Posn
    (define (radius-now cx cy)
      (sqrt (+ (sqr (- cx (posn-x center))) (sqr (- cy (posn-y center))))))
    
    ; draw : Image -> Image
    ; Returns an image of the circle
    ; Strategy: data decompostion on state: ShapeState
    (define/public (draw background)
      (cond
        [(creating? state) (place-image (circle radius 127 "red") (get-centerX)
                                        (get-centerY) background)]
        [else (place-image (circle radius "outline" "black") (get-centerX)
                                        (get-centerY) background)]))
    
    ; get-centerX : -> Coordinate
    ; Returns the x coordinate of the center of the circle
    ; Strategy: Data decomposition on center : Posn
    (define (get-centerX) 
      (posn-x center))
    
    ; get-centerY : -> Coordinate
    ; Returns the y coordinate of the center of the circle
    ; Strategy: Data decomposition on center : Posn
    (define (get-centerY) 
      (posn-y center))
    
    ; equal-to? : Circle% -> Boolean
    (define/public (equal-to? other)
      (and (is-a? other Circle%)
           (= (hash-code) (send other hash-code))))
    
    ; hash-code: -> Number
    (define/public (hash-code)
      (+(* 4398042316799 (posn-x center))
        (* 274876858367 (posn-y center))
        radius
        (hash/shape-state state)))
    
    ; test:to-string: -> String
    (define/public (test:to-string)
      (string-append "center" (~v center)
                     "| radius" (~v radius)
                     "| state: " state
                     "| v/s" (~v v/s)
                     "| v/c" (~v v/c)))))

; Triangle% : A class that satisfies the Shape<%> Drawable<%> Comparable<%>
;            ToString<%> interface
; A Triangle is a (new Triangle% [center Posn] [dynamic Posn] 
;                [state ShapeState] [v/s Posn] [v/c Posn] [v/d Posn])
; INTERP : Represents a triangle, with a stable and dynamic point, state
; , vector start point, vector center, and vector dynamic point.
(define Triangle%
  (class* object% (Shape<%> Drawable<%> Comparable<%> ToString<%>)
    (init-field center dynamic state v/s v/c v/d) 
    ; INTERPRETATION: 'center' is the posn for center of the triangle,
    ; 'radius' is the positive integer for the triangle which changes 
    ; during the drag event, 
    ; 'state' is a ShapeState that represents the current state of the shape
    ; 'v/s' is a posn for vector for moving the shape
    ; 'v/c' is a posn for initial center wihle 'v/d' is for initial dynamic 
    ; point
    
    (super-new)
    
    ; the limitation of the control region boundary
    (define BNDY 5)
    
    ; get-bounds : -> BoundingBox
    ; Returns a list of coordinate representing a triangle
    ; Startegy: Data decomposition on center : Posn
    (define/public (get-bounds) 
      (local((define cnrs (compute-corners center dynamic))
             (define c1 (first cnrs))
             (define c2 (second cnrs))
             (define c3 (third cnrs)))
        (list (min (posn-x c1) (posn-x c2) (posn-x c3))
              (min (posn-y c1) (posn-y c2) (posn-y c3))
              (max (posn-x c1) (posn-x c2) (posn-x c3))
              (max (posn-y c1) (posn-y c2) (posn-y c3))))) 
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Triangle%
    ; Returns the object of the class depending upon the mouse event
    ; Startegy: Data decomposition on e : MouseEvent
    (define/public (handle-mouse cx cy e) 
      (cond
        [(string=? e "button-down") (gesture-start cx cy)]
        [(string=? e "drag") (gesturing cx cy)]
        [(string=? e "button-up") (gesture-end cx cy)]))
    
    ; gesture-start : Coordinate Coordinate -> Triangle%
    ; Returns a new triangle by checking the state of the object
    ; Strategy: Data decomposition on state: ShapeState
    (define (gesture-start cx cy)
      (cond
        [(or (moving? state) (resizing? state) (creating? state)) 
         (build/t (make-posn cx cy) (make-posn cx cy) state v/s v/c v/d)]
        [(created? state) (init/resize/move cx cy)]))
    
    ; init/resize/move : Coordinate Coordinate -> Triangle%
    ; Init resizing or moving a new triangle depending on the current state of 
    ; object
    (define (init/resize/move cx cy)
      (local((define controlable (controlable? cx cy)))
        (if controlable
            (init-resize cx cy)
            (if (inside/this? cx cy)
                (init-move cx cy)
                this))))
    
    ; controlable? : Coordinate Coordinate -> Boolean
    ; Returns true if the mouse click is within the control boundary
    (define (controlable? cx cy)
      (local((define coners (compute-corners center dynamic)))
        (ormap (lambda (co) (inside? co cx cy BNDY BNDY)) coners)))
    
    ; inside/this? : Coordinate Coordinate -> Boolean
    ; Returns true if the mouse click is inside the triangle
    (define (inside/this? cx cy)
      (local ((define coners (compute-corners center dynamic)))
        (in-triangle? (make-posn cx cy) 
                      (first coners) (second coners) (third coners))))
    
    ; init-resize : Coordinate Coordinate -> Triangle% 
    ; Returns a new resized triangle by changing the radius
    (define (init-resize cx cy)
      (build/t center (make-posn cx cy) resizing v/s v/c v/d))
    
    ; init-move : Coordinate Coordinate -> Triangle% 
    ; Returns a new moved triangle by changing the vector position
    (define (init-move cx cy)
      (build/t center dynamic moving (make-posn cx cy) center dynamic))
    
    ; gesturing : Coordinate Coordinate -> Triangle%
    ; Returns a new triangle based on the state of the triangle
    ; Strategy: Data decomposition on state: ShapeState
    (define (gesturing cx cy)
      (cond
        [(or (resizing? state) (creating? state)) 
         (build/t center (make-posn cx cy) state v/s v/c v/d)]
        [(moving? state) (add/vector cx cy)]
        [(created? state) this]))
    
    ; gesture-end : Coordinate Coordinate -> Triangle%
    ; Returns a new triangle with state as created
    (define (gesture-end cx cy)
      (build/t center dynamic created v/s v/c v/d))
    
    ; add/vector : Coordinate Coordinate -> Triangle%
    ; Returns a new cirlce with center updated according to the 
    ; coordinates passed and initial center posn.
    (define (add/vector cx cy)
      (local((define vector (subtract-posn (make-posn cx cy) v/s))
             (define new/center (add-posn vector v/c))
             (define new/dyn (add-posn vector v/d)))
        (build/t new/center new/dyn state v/s v/c v/d)))
    
    ; draw : Image -> Image
    ; Returns an image of the triangle
    ; Strategy: data decompostion on state: ShapeState
    (define/public (draw background)
      (cond
        [(string=? creating state)
         (render-equilateral-triangle center dynamic 127 "red" 
                                      background)]
        [else 
         (render-equilateral-triangle center dynamic "outline" "black" 
                                      background)]))
    
    ; equal-to?: Triangle% -> Boolean
    (define/public (equal-to? other)
      (and (is-a? other Triangle%)
           (= (hash-code) (send other hash-code))))
    
    ; hash-code : -> Number
    (define/public (hash-code)
      (+(* 4398042316799 (posn-x center))
        (* 16769023 (posn-x dynamic))
        (* 274876858367 (posn-y center))
        (* 68718952447 (posn-y dynamic))
        (hash/shape-state state)))
    
    ; test:to-string: -> String
    (define/public (test:to-string)
      (string-append "center" (~v center) 
                     "| dynamic" (~v dynamic) 
                     "| state: " state 
                     "| v/s"(~v v/s) 
                     "| v/c"(~v v/c)
                     "| v/d" (~v v/d)))))

; A World is a 
; make-world(ListOf<Shape<%>> DrawState Boolean)
; INTERP: shapes represents all the shapes in the current world, state represent
; the DrawState of the current world, stop-working represents whether the 
; current world is stop working
(define-struct world (shapes state stop-working?))
; INITIAL-WORLD : World
; An initial world, with no Shape<%>s.
(define INITIAL-WORLD (make-world '() pointer #false))

; Data examples:
(define posn1 (make-posn 50 50))
(define posn2 (make-posn 200 200))
(define rec1 (new Rectangle% [stable posn1] [dynamic posn2] 
                  [state created] [v/s VS] [v/c VS]))
(define rec2 (new Rectangle% [stable posn1] [dynamic posn2]
                  [state creating] [v/s VS] [v/c VS]))
(define rec3 (new Rectangle% [stable posn1] [dynamic posn2] 
                  [state moving] [v/s VS] [v/c VS]))
(define rec4 (new Rectangle% [stable posn1] [dynamic posn2]
                  [state resizing] [v/s VS] [v/c VS]))
(define cir0 
  (new Circle% [center POSN] [radius 0] [state created] [v/s VS] [v/c VS]))
(define cir1 
  (new Circle% [center posn1] [radius 150] [state created] [v/s VS] [v/c VS]))
(define cir2 
  (new Circle% [center posn1] [radius 150] [state creating] [v/s VS] [v/c VS]))
(define cir3 
  (new Circle% [center posn1] [radius 150] [state moving] [v/s VS] [v/c VS]))
(define cir4 
  (new Circle% [center posn1] [radius 150] [state resizing] [v/s VS] [v/c VS]))
(define tri1 
  (new Triangle% [center posn1] [dynamic posn2] [state created] 
       [v/s VS] [v/c VS] [v/d VS]))
(define tri2 
  (new Triangle% [center posn1] [dynamic posn2] [state creating] 
       [v/s VS] [v/c VS] [v/d VS]))
(define tri3 
  (new Triangle% [center posn1] [dynamic posn2] [state resizing] 
       [v/s VS] [v/c VS] [v/d VS]))
(define tri4 
  (new Triangle% [center posn1] [dynamic posn2] [state moving] 
       [v/s VS] [v/c VS] [v/d VS]))
(define tri5 
  (new Triangle% [center posn1] [dynamic posn2] [state resizing] 
       [v/s VS] [v/c VS] [v/d VS]))

(define world1 (make-world (list cir0) pointer #false))
(define world2 (make-world (list cir1) pointer #true))
(define world3 (make-world (list cir1 cir2 cir3 cir4 tri1 tri2 tri3 tri4 tri5
                                 rec1 rec2 rec3 rec4) pointer #false))
(define world4 (make-world (list cir1 cir2 cir3 cir4 tri1 tri2 tri3 tri4 tri5
                                 rec1 rec2 rec3 rec4) rec #false))
(define world5 (make-world (list cir1 cir2 cir3 cir4 tri1 tri2 tri3 tri4 tri5
                                 rec1 rec2 rec3 rec4) cir #false))
(define world6 (make-world (list cir1 cir2 cir3 cir4 tri1 tri2 tri3 tri4 tri5
                                 rec1 rec2 rec3 rec4) tri #false))

; build/r : Posn Posn ShapeState Posn -> Rectangle%
; Returns a new object of rectangle class
; Strategy: Function compostion
(begin-for-test
  (check-true (object? (build/r POSN POSN created VS VS))))
(define (build/r s d state vs vc)
  (new Rectangle% [stable s] [dynamic d] [state state] [v/s vs] [v/c vc]))

; build/c : Posn PosInt ShapeState Posn -> Circle%
; Returns a new object of circle class
; Strategy: Function compostion
(begin-for-test
  (check-true (object? (build/c POSN 1 created VS VS))))
(define (build/c cntr r s vs vc)
  (new Circle% [center cntr] [radius r] [state s] [v/s vs] [v/c vc]))

; build/t : Posn Posn ShapeState Posn -> Triangle%
; Returns a new object of Triangle% class
; Strategy: Function compostion
(begin-for-test
  (check-true (object? (build/t POSN POSN created VS VS VS))))
(define (build/t c d state vs vc vd)
  (new Triangle% 
       [center c] [dynamic d] [state state] [v/s vs] [v/c vc][v/d vd]))

; inside? : Posn Coordinate Coordinate Integer Integer -> Boolean
; Returns true if the mouse click (x, y) is inside the rectangle,
; which defined by (center, dis-x, dis-y) where center is the center of
; the rectanle, dis-x, dis-y represents the distance to the center.
; Strategy: Data decomposition on center : Posn
(begin-for-test
  (check-false (inside? POSN 4 100 5 5)))
(define (inside? center x y dis-x dis-y)
  (and (<= (abs (- x (posn-x center))) dis-x) 
       (<= (abs (- y (posn-y center))) dis-y)))

; draw-world : World -> Image
; Returns an image consisting of the draw area and toolbar
; Strategy: Function composition
(begin-for-test
  (check-equal? (draw-world (make-world '() pointer #false))
                (overlay/align "left" "top" 
                               (above (overlay WHITE-P BLACK-SQUARE)
                                      (overlay BLACK-R WHITE-SQUARE)
                                      (overlay BLACK-C WHITE-SQUARE)
                                      (overlay BLACK-T WHITE-SQUARE))
                               MT)
                "Test failed. Function should return image of empty scene with 
toolbar and pointer selected")
  (check-equal? (draw-world (make-world '() rec #false))
                (overlay/align "left" "top" 
                               (above (overlay BLACK-P WHITE-SQUARE)
                                      (overlay WHITE-R BLACK-SQUARE)
                                      (overlay BLACK-C WHITE-SQUARE)
                                      (overlay BLACK-T WHITE-SQUARE))
                               MT)
                "Test failed. Function should return image of empty scene with 
toolbar and rectangle selected")
  (check-equal? (draw-world (make-world '() cir #false))
                (overlay/align "left" "top" 
                               (above (overlay BLACK-P WHITE-SQUARE)
                                      (overlay BLACK-R WHITE-SQUARE)
                                      (overlay WHITE-C BLACK-SQUARE)
                                      (overlay BLACK-T WHITE-SQUARE))
                               MT)
                "Test failed. Function should return image of empty scene with 
toolbar and circle selected")
  (check-equal? (draw-world (make-world '() tri #false))
                (overlay/align "left" "top" 
                               (above (overlay BLACK-P WHITE-SQUARE)
                                      (overlay BLACK-R WHITE-SQUARE)
                                      (overlay BLACK-C WHITE-SQUARE)
                                      (overlay WHITE-T BLACK-SQUARE))
                               MT)
                "Test failed. Function should return image of empty scene with 
toolbar and circle selected"))
(define (draw-world w)
  (overlay/align "left" "top"
                 (draw-toolbar w)
                 (draw-all-shapes w)))

; draw-toolbar: World -> Image
; Draws different toolbar states based on the current world's state.
; Strategy: Data decomposition on w : World
(begin-for-test
  (check-equal? (draw-toolbar INITIAL-WORLD)
                (above (overlay WHITE-P BLACK-SQUARE)
                       (overlay BLACK-R WHITE-SQUARE)
                       (overlay BLACK-C WHITE-SQUARE)
                       (overlay BLACK-T WHITE-SQUARE))))
(define (draw-toolbar w)
  (cond
    [(pointer? (world-state w)) (above (overlay WHITE-P BLACK-SQUARE)
                                       (overlay BLACK-R WHITE-SQUARE)
                                       (overlay BLACK-C WHITE-SQUARE)
                                       (overlay BLACK-T WHITE-SQUARE))]
    [(rectangle? (world-state w)) (above (overlay BLACK-P WHITE-SQUARE)
                                         (overlay WHITE-R BLACK-SQUARE)
                                         (overlay BLACK-C WHITE-SQUARE)
                                         (overlay BLACK-T WHITE-SQUARE))]
    [(circle? (world-state w)) (above (overlay BLACK-P WHITE-SQUARE)
                                      (overlay BLACK-R WHITE-SQUARE)
                                      (overlay WHITE-C BLACK-SQUARE)
                                      (overlay BLACK-T WHITE-SQUARE))]
    [else (above (overlay BLACK-P WHITE-SQUARE)
                 (overlay BLACK-R WHITE-SQUARE)
                 (overlay BLACK-C WHITE-SQUARE)
                 (overlay WHITE-T BLACK-SQUARE))]))

; draw-all-shapes : World -> Image
; Draws all the shapes in the current world.
; Strategy: Function composition
(begin-for-test
  (check-true (image? (draw-all-shapes world3))
              MT))
(define (draw-all-shapes w)
  (foldr (lambda (s image) (send s draw image)) MT (world-shapes w)))

; handle-mouse : World Coordinate Coordinate MouseEvent -> World
; GIVEN: A World, mouse coordinates, and a MouseEvent
; RETURNS: A new World, like the given one, updated to reflect the action of
;    the mouse event, in the ways specified in the problem set.
; Strategy: Data decomposition on e : MouseEvent
(begin-for-test
  (check-equal? (handle-mouse world1 15 15 "move")
                world1
                "Test failed. Function should return a new world")
  (check-equal? (handle-mouse world1 15 15 "enter")
                world1
                "Test failed. Function should return a new world")
  (check-equal? (handle-mouse world1 15 15 "leave")
                world1
                "Test failed. Function should return a new world")
  (check-true (world? (handle-mouse world2 15 15 "button-up"))
              "Test failed. Function should return a new world")
  (check-true (world? (handle-mouse world1 15 15 "button-down"))
              "Test failed. Function should return a new world"))
(define (handle-mouse w c1 c2 e)
  (cond
    [(or (string=? e "move") (string=? e "enter") (string=? e "leave")) w]
    [else (if (world-stop-working? w)
              (handle-stop-working w e)
              (handle-world w c1 c2 e))]))

; handle-stop-working : World MouseEvent -> World
; Returns a world with world working as true or false 
; depending on mopuse event.
; Strategy: Data decomposition on e : MouseEvent
(begin-for-test
  (check-true (world? (handle-stop-working world2 "button-up"))
              "Test failed. Function should return a new world")
  (check-true (world? (handle-stop-working world2 "button-down"))
              "Test failed. Function should return a new world"))
(define (handle-stop-working w e)
  (cond
    [(string=? "button-up" e) 
     (make-world (world-shapes w) (world-state w) #false)]
    [else w]))

; handle-world : World Coordinate Coordinate MouseEvent -> World
; Returns a world by checking if mouse click is on toolbar or draw area
; Strategy: Data decomposition on e: MouseEvent
(begin-for-test
  (check-true (world? (handle-world world1 10 10 "button-down")))
  (check-true (world? (handle-world world1 100 100 "button-down"))))
(define (handle-world w c1 c2 e)
  (if (inside/toolbar? c1 c2) 
      (handle-toolbar w c1 c2 e)
      (handle-shapes w c1 c2 e)))

; inside/toolbar?: Coordinate Coordinate -> Boolean
; Returns true if the click is within toolbar area.
; Strategy: Function composition
(define (inside/toolbar? c1 c2)
  (and (<= TOOLBAR-LEFT c1 TOOLBAR-RIGHT) 
       (<= TOOLBAR-TOP c2 TOOLBAR-BOTTOM)))

; handle-toolbar : World Coordinate Coordinate MouseEvent -> World
; Returns a world with a tool selected from toolbar depending upon the 
; mouse event and moiuse position
; Strategy: Data decomposition on e : MouseEvent
(begin-for-test
  (check-true (world? (handle-world world1 10 10 "drag")))
  (check-true (world? (handle-world world1 10 10 "button-up"))))
(define (handle-toolbar w c1 c2 e)
  (cond 
    [(string=? "drag" e) 
     (make-world (world-shapes w) (world-state w) #true)]
    [(string=? "button-up" e)
     (make-world (world-shapes w) (next-state c1 c2) #false)]
    [else w]))

; next-state : Coordinate Coordinate -> DrawState
; GIVEN: A World, mouse coordinates, and a MouseEvent
; RETURNS: A new World, like the given one, updated to reflect the action of
;    the mouse event, in the ways specified in the problem set.
(begin-for-test
  (check-equal? (next-state 5 5) pointer 
                "Test failed. Function should return a pointer")
  (check-equal? (next-state 5 25) rec 
                "Test failed. Function should return a rectangle")
  (check-equal? (next-state 5 45) cir 
                "Test failed. Function should return a circle")
  (check-equal? (next-state 5 65) tri 
                "Test failed. Function should return a circle")) 
(define (next-state c1 c2)
  (if (and (<= TOOLBAR-LEFT c1 TOOLBAR-RIGHT) 
           (<= TOOLBAR-TOP c2 BETWEEN/P/R))
      pointer
      (if (and (<= TOOLBAR-LEFT c1 TOOLBAR-RIGHT) 
               (<= BETWEEN/P/R c2 BETWEEN/R/C))
          rec
          (if (and (<= TOOLBAR-LEFT c1 TOOLBAR-RIGHT) 
                   (<= BETWEEN/R/C c2 BETWEEN/C/T))
              cir
              tri))))

; handle-shapes : World Coordinate Coordinate MouseEvent -> World
; Computes next world state based on the current and the mouse event
; Strategy: Data decompostion on w : World
(begin-for-test
  (check-true (world? (handle-shapes world3 200 200 "drag")))
  (check-true (world? (handle-shapes world3 200 200 "button-up")))
  (check-true (world? (handle-shapes world3 200 200 "button-down")))
  (check-true (world? (handle-shapes world3 100 100 "button-down")))
  (check-true (world? (handle-shapes world4 200 200 "drag")))
  (check-true (world? (handle-shapes world4 200 200 "button-up")))
  (check-true (world? (handle-shapes world4 200 200 "button-down")))
  (check-true (world? (handle-shapes world4 100 100 "button-down")))
  (check-true (world? (handle-shapes world4 500 500 "button-down")))
  (check-true (world? (handle-shapes world5 50 200 "drag")))
  (check-true (world? (handle-shapes world5 50 200 "button-up")))
  (check-true (world? (handle-shapes world5 50 200 "button-down")))
  (check-true (world? (handle-shapes world5 100 100 "button-down")))
  (check-true (world? (handle-shapes world5 500 500 "button-down")))
  (check-true (world? (handle-shapes world6 200 200 "drag")))
  (check-true (world? (handle-shapes world6 200 200 "button-up")))
  (check-true (world? (handle-shapes world6 200 200 "button-down")))
  (check-true (world? (handle-shapes world6 100 100 "button-down")))
  (check-true (world? (handle-shapes world6 500 500 "button-down"))))
(define (handle-shapes w c1 c2 e)
  (cond 
    [(pointer? (world-state w)) 
     (make-world (handle-pointer (world-shapes w) c1 c2 e) pointer #false)]
    [(rectangle? (world-state w)) 
     (make-world (handle-draw (world-shapes w) c1 c2 e 
                              (build/r POSN POSN creating VS VS)) 
                 rec #false)]
    [(circle? (world-state w))
     (make-world (handle-draw (world-shapes w) c1 c2 e 
                              (build/c POSN 0 creating VS VS))
                 cir #false)]
    [else (make-world (handle-draw (world-shapes w) c1 c2 e 
                                   (build/t POSN POSN creating VS VS VS))
                      tri #false)]))

(define (handle-pointer shapes c1 c2 e)
  (modify c1 c2 e shapes))

; handle-draw : ListOf<Shape<%>> Coordinate Coordinate MouseEvent Shape<%> -> 
; ListOf<Shape<%>>
; Returns all the shapes of the world after a mouse event intented to draw
; a new shape
; Strategy: Function composition
(begin-for-test
  (check-true 
   (list? (handle-draw '() 100 100 "button-down" 
                       (new Circle% [center POSN] [radius 0] [state creating]
                            [v/s VS] [v/c VS])))))
(define (handle-draw shapes c1 c2 e new-instance)
  (local((define creating-shapes (filter-creating shapes)))
    (append (modify c1 c2 e (if (empty? creating-shapes) 
                                (list new-instance)
                                creating-shapes))
            (filter-created shapes))))

; filter-creating: ListOf<Shape<%>> -> ListOf<Shape<%>>
; Returns the those shapes in the current world whose state is creating
; Strategy: Function composition
(begin-for-test
  (check-equal? (filter-creating (list rec1 rec2))
                (list rec2)))
(define (filter-creating shapes)
  (filter (lambda (s) (creating? (get-field state s)))
          shapes))

; filter-created ListOf<Shape<%>> -> ListOf<Shape<%>>
; Returns the those shapes in the current world whose state is created
; Strategy: Function composition
(begin-for-test
  (check-equal? (filter-created (list rec1 rec2))
                (list rec1)))
(define (filter-created shapes)
  (filter (lambda (s) (created? (get-field state s)))
          shapes))

; modify: Coordinate Coordinate MouseEvent ListOf<Shape<%>> -> ListOf<Shape<%>>
; Computes new shapes based on the given click(c1, c2) and MouseEvent e
; Strategy: Function composition
(begin-for-test
  (check-true (list? (modify 100 100 "drag" (list rec1 rec2 rec3 rec4
                                                  cir1 cir2 cir3 cir4)))))
(define (modify c1 c2 e shapes)
  (map (lambda (s) (send s handle-mouse c1 c2 e))
       shapes))

; get-world-shapes : World -> ListOf<Shape<%>>
; GIVEN: A World,
; RETURNS: All the Shape<%>s which make up that world, i.e. all those that
;    have been created by the user through using the tools.
; Strategy: Function composition
(begin-for-test
  (check-equal? (get-world-shapes world1) 
                (list cir0)))
(define (get-world-shapes w) 
  (filter (lambda (s) (created? (get-field state s)))
          (world-shapes w)))

; create-circle : Posn Integer -> Shape<%>
; GIVEN: A center point and a radius
; RETURNS: A new Circle% object (implementing Shape<%>) with its center at
;    the given point and radius as given.
; Strategy: Function composition
(begin-for-test
  (check-true (object? (create-circle POSN 1))))
(define (create-circle pos r)
  (build/c pos r creating VS VS))

; create-rectangle : BoundingBox -> Shape<%>
; GIVEN: A bounding box,
; RETURNS: A new Rectangle% object (implementing Shape<%>) which is bounded
;    by the given BoundingBox.
; Strategy: Function composition
(begin-for-test
  (check-true (object? (create-rectangle (list 1 2 3 4)))))
(define (create-rectangle b)
  (build/r (make-posn (first b) (second b))
           (make-posn (third b) (fourth b))
           created VS VS))

; create-triangle : posn posn -> Shape<%>
; Creates a triangle Shape<%> object from a center
; posn and one arbitrary corner.
(begin-for-test
  (check-true (object? (create-triangle POSN POSN))))
(define (create-triangle center corner)
  (build/t center corner created VS VS VS))

; hash/shape-state ShapeState -> Number
; Return the hashCode of shape-state
; Strategy: Data decomposition on ss : ShapeState
(begin-for-test
  (check-equal? (hash/shape-state created)
                2))
(define (hash/shape-state ss)
  (cond
    [(created? ss) 2]
    [(creating? ss) 3]
    [(moving? ss) 5]
    [(resizing? ss) 7]))

; run: World -> Image
; Computes next world state based on the current world state
; Strategy: function composition
(define (run w)
  (big-bang w
            (on-mouse handle-mouse)
            (to-draw draw-world)))

; Tests:
; Check get-bounds methods
(begin-for-test
  (check-equal? (send rec1 get-bounds)
                '(50 50 200 200))
  (check-equal? (send cir1 get-bounds)
                '(-100 -100 200 200))
  (check-equal? (send tri1 get-bounds)
                '(-154.90381056766577 -154.90381056766586 200.0 200.0)))

; Check to-string method
(begin-for-test
  (check-true (string? 
               (send (send cir0 handle-mouse 0 0 "button-down")
                     test:to-string)))
  (check-true (string? 
               (send (send rec1 handle-mouse 500 500 "button-down") 
                     test:to-string)))
  (check-true (string? 
               (send 
                (send tri1 handle-mouse 500 500 "button-down") 
                test:to-string))))

; Check handle-mouse
(begin-for-test
  (check-true (send (send cir0 handle-mouse 0 0 "button-down") equal-to? 
                    (build/c (make-posn 0 0) 0 resizing 
                             (make-posn 0 0) (make-posn 0 0))))
  (check-true (send (send rec1 handle-mouse 500 500 "button-down") equal-to? 
                    (build/r (make-posn 50 50) (make-posn 200 200) created 
                             (make-posn 0 0) (make-posn 0 0))))
  (check-true (send (send cir2 handle-mouse 200 200 "button-down") equal-to?
                    (build/c (make-posn 200 200) 0 creating 
                             (make-posn 0 0) (make-posn 0 0))))
  (check-true (send (send cir3 handle-mouse 100 100 "button-down") equal-to? 
                    (build/c (make-posn 100 100) 0 moving 
                             (make-posn 0 0) (make-posn 0 0))))
  (check-true (send (send tri1 handle-mouse 100 100 "button-down") equal-to? 
                    (build/t (make-posn 50 50) (make-posn 200 200) moving 
                             (make-posn 100 100) (make-posn 50 50) 
                             (make-posn 200 200))))
  (check-true (send (send tri2 handle-mouse 100 100 "drag") equal-to? 
                    (build/t (make-posn 50 50) (make-posn 100 100) creating 
                             (make-posn 0 0) (make-posn 0 0) 
                             (make-posn 0 0))))
  (check-true (send (send tri3 handle-mouse 100 100 "drag") equal-to? 
                    (build/t (make-posn 50 50) (make-posn 100 100) resizing 
                             (make-posn 0 0) (make-posn 0 0) 
                             (make-posn 0 0))))
  (check-true (send (send tri4 handle-mouse 100 100 "button-up") equal-to? 
                    (build/t (make-posn 50 50) (make-posn 200 200) created 
                             (make-posn 0 0) (make-posn 0 0) 
                             (make-posn 0 0)))))