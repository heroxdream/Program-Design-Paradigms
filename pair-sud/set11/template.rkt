#lang racket
(require "extras.rkt")
(require rackunit)
(require lang/posn)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 10)

;(provide INITIAL-WORLD)
;(provide handle-mouse)
;(provide Shape<%>)
;(provide get-world-shapes)
;(provide create-rectangle)
;(provide create-circle)

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

; A BoundingBox is a (list Coordinate Coordinate Coordinate Coordinate)
; INTERPRETATION: (list left top right bottom).
; A BoundingBox represents a box whose left x-coordinate is at "left", whose
; top y-coordinate is at "top", whose right x-coordinate is at "right", and 
; whose bottom y-coordinate is at "bottom".


; INITIAL-WORLD : World
; An initial world, with no Shape<%>s.

; A ShapeState is one of:
; - created
; - creating
; - moving
; - resizing
(define created "created")
(define creating "creating")
(define moving "moving")
(define resizing "resizing")
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
; - none
(define pointer "pointer")
(define rec "rectangle")
(define cir "circle")
(define none "none")
(define (pointer? state)
  (string=? pointer state))
(define (rectangle? state)
  (string=? rec state))
(define (circle? state)
  (string=? cir state))
(define (none? state)
  (string=? none state))


(define Shape<%>
  (interface ()
    ; get-bounds : -> BoundingBox
    get-bounds
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Shape<%>
    handle-mouse))

(define Drawable<%>
  (interface ()
    ; draw -> Image
    draw
    
    ; get-center -> Posn
    get-centerX
    get-centerY))

(define Rectangle%
  (class* object% (Shape<%> Drawable<%>)
    
    (init-field left top right bottom state start)
    
    (define/public (set-left l)
      (set-field! left this l))
    (define/public (set-top t)
      (set-field! top this t))
    (define/public (set-right r)
      (set-field! right this r))
    (define/public (set-bottom b)
      (set-field! bottom this b))
    
    (define CONTROL-BOUNDARY 5)
    (define ILEFT left)
    (define ITOP top)
    (define IRIGHT right)
    (define IBOTTOM bottom)
    
    (define/public (get-bounds) (list left top right bottom))
    
    (define/public (handle-mouse cx cy e) 
      (cond 
        [(string=? e "button-down") (gesture-start cx cy)]
        [(string=? e "drag") (gesturing cx cy)]
        [(string=? e "button-up") (gesture-end cx cy)]
        [else this]))
    
    (define (gesture-start cx cy)
      (cond
        [(or (moving? state) (resizing? state)) (error "illegal state")]
        [(creating? state) 
         (build cx cy cx cy state (make-posn cx cy))]
        [(created? state) (init/resize/move cx cy)]))
    
    (define (init/resize/move cx cy)
      (local((define stable (control-posn cx cy)))
        (if (posn? stable)
            (init-resize stable cx cy)
            (if (inside/this? cx cy)
                (init-move cx cy)
                this))))
    
    (define (init-resize stable cx cy)
      (build (min cx (posn-x stable))
             (min cy (posn-y stable))
             (max cx (posn-x stable))
             (max cy (posn-y stable))
             resizing stable))
    
    (define (init-move cx cy)
      (build left top right bottom moving (make-posn cx cy)))
    
    (define (gesturing cx cy)
      (cond
        [(or (resizing? state) (creating? state))(build (min cx (posn-x start))
                                                        (min cy (posn-y start))
                                                        (max cx (posn-x start))
                                                        (max cy (posn-y start))
                                                        state start)]
        [(moving? state) (add/vector cx cy)]
        [(created? state) this]))
    
    (define (add/vector cx cy)
      (local((define v/x (- cx (posn-x start)))
             (define v/y (- cy (posn-y start)))
             (define v (send* this 
                         (set-left (+ ILEFT v/x))
                         (set-top (+ ITOP v/y))
                         (set-right (+ IRIGHT v/x))
                         (set-bottom (+ IBOTTOM v/y)))))
        this))
    
    (define (gesture-end cx cy)
      (build left top right bottom created (make-posn 0 0)))

    ; -> Maybe<Posn>
    (define (control-posn cx cy)
      (local((define coner1 (make-posn left top))
             (define coner2 (make-posn right top))
             (define coner3 (make-posn left bottom))
             (define coner4 (make-posn right bottom))
             (define coners (list coner1 coner2 coner3 coner4))
             (define hashmap (make-hash (list (list coner1 coner4) 
                                              (list coner2 coner3)
                                              (list coner3 coner2)
                                              (list coner4 coner1))))
             (define control-region 
               (filter (lambda (c) (inside? c cx cy 
                                            CONTROL-BOUNDARY CONTROL-BOUNDARY))
                       coners)))
        (if (empty? control-region)
            #false
            (first (hash-ref hashmap (first control-region))))))
    
    (define (inside? center x y dis-x dis-y)
      (and (< (abs (- x (posn-x center))) dis-x) 
           (< (abs (- y (posn-y center))) dis-y)))
    
    (define (inside/this? cx cy)
      (inside? (make-posn (get-centerX) (get-centerY))
               cx cy 
               (/ (- right left) 2) (/ (- bottom top) 2)))
    
    
    
    (define/public (draw) 
      (cond
        [(string=? creating state) 
         (rectangle (- right left) (- bottom top) 127 "red")]
        [else (rectangle (- right left) (- bottom top) "outline" "black")]))
    
    (define/public (get-centerX) 
      (/ (+ left right) 2))
    
    (define/public (get-centerY) 
      (/ (+ top bottom) 2))
    
    (super-new)))


(define (build l t r b state s)
  (new Rectangle% 
       [left l] [top t] [right r] [bottom b] [state state] [start s]))

(define rec1 
  (new Rectangle% 
       [left 10]
       [top 100]
       [right 200]
       [bottom 200]
       [state created]
       [start (make-posn 0 0)]))

(define Circle%
  (class* object% (Shape<%> Drawable<%>)
    (init-field x y radius state)
    
    (define/public (get-bounds) 0)
    
    (define/public (handle-mouse cx cy e) 
      (cond
        [(creating? state) (handle-creating cx cy e)]
        [else this]))
    
    (define (handle-creating cx cy e)
      (cond
        [(string=? e "button-down")(new this% 
                                        [x cx]
                                        [y cy]
                                        [radius 0]
                                        [state creating])]
        [(string=? e "drag") (new this% 
                                  [x x]
                                  [y y]
                                  [radius (radius-now cx cy)]
                                  [state creating])]
        [(string=? e "button-up") (new this% 
                                       [x x]
                                       [y y]
                                       [radius radius]
                                       [state created])]
        [else this]))
    
    (define (radius-now cx cy)
      (sqrt (+ (sqr (- cx x)) (sqr (- cy y)))))
    
    (define/public (draw) 
      (cond
        [(string=? created state) (circle radius "outline" "black")]
        [(string=? creating state) (circle radius 127 "red")]))
    
    (define/public (get-centerX) 
      x)
    
    (define/public (get-centerY) 
      y)
    
    (super-new)))
(define cir1 
  (new Circle% 
       [x 200]
       [y 200]
       [radius 100]
       [state created]))


; A World is a 
; make-world(ListOf<Shape<%>> DrawState)
; WHERE: (length rec) ≤ 1 AND (length cir) ≤ 1 
(define-struct world (shapes state))

(define INITIAL-WORLD (make-world (list cir1 rec1) rec))


(define (run w)
  (big-bang w
            (on-mouse handle-mouse)
            (to-draw draw-world)))


(define (draw-world w)
  (overlay/align "left" "top"
                 (draw-toolbar w)
                 (draw-all-shapes w)))

(define (draw-toolbar w)
  (above (overlay BLACK-P WHITE-SQUARE)
         (overlay BLACK-R WHITE-SQUARE)
         (overlay BLACK-C WHITE-SQUARE)))

(define (draw-all-shapes w)
  (foldr (lambda (s image) (place-image (send s draw)
                                        (send s get-centerX)
                                        (send s get-centerY)
                                        image))
         MT
         (world-shapes w)))


; handle-mouse : World Coordinate Coordinate MouseEvent -> World
; GIVEN: A World, mouse coordinates, and a MouseEvent
; RETURNS: A new World, like the given one, updated to reflect the action of
;    the mouse event, in the ways specified in the problem set.
(define (handle-mouse w c1 c2 e)
  (cond 
    [(pointer? (world-state w)) 
     (make-world (handle-pointer (world-shapes w) c1 c2 e) pointer)]
    [(rectangle? (world-state w)) 
     (make-world (handle-draw-rectangle (world-shapes w) c1 c2 e) rec)]
    [(circle? (world-state w)) 
     (make-world (handle-draw-circle (world-shapes w) c1 c2 e) cir)]
    [else w])) 

(define (handle-pointer shapes c1 c2 e)
  (local(;
         (define (modify c1 c2 e shapes)
           (map (lambda (s) (send s handle-mouse c1 c2 e))
                shapes)))
    (modify c1 c2 e shapes)))

; ListOf<Shape<%>> Coordinate Coordinate MouseEvent -> ListOf<Shape<%>>
(define (handle-draw-rectangle shapes c1 c2 e)
  (local((define creating-shapes (filter-creating shapes))
         ;
         (define (modify c1 c2 e shapes)
           (map (lambda (s) (send s handle-mouse c1 c2 e))
                shapes)))
    (append (modify c1 c2 e (if (empty? creating-shapes) 
                                (list (init-rectangle c1 c2))
                                creating-shapes)) 
            (filter-created shapes))))



(define (filter-creating shapes)
  (filter (lambda (s) (creating? (get-field state s)))
          shapes))

(define (filter-created shapes)
  (filter (lambda (s) (created? (get-field state s)))
          shapes))


(define (handle-draw-circle shapes c1 c2 e)
  (local((define creating-shapes (filter-creating shapes))
         ;
         (define (modify c1 c2 e shapes)
           (map (lambda (s) (send s handle-mouse c1 c2 e))
                shapes)))
    (append (modify c1 c2 e (if (empty? creating-shapes) 
                                (list (create-circle (make-posn c1 c2) 0))
                                creating-shapes)) 
            (filter-created shapes))))

; get-world-shapes : World -> ListOf<Shape<%>>
; GIVEN: A World,
; RETURNS: All the Shape<%>s which make up that world, i.e. all those that
;    have been created by the user through using the tools.
(define (get-world-shapes w) 
  (filter (lambda (s) (created? (get-field state s)))
          (world-shapes w)))

; create-circle : Posn Integer -> Shape<%>
; GIVEN: A center point and a radius
; RETURNS: A new Circle% object (implementing Shape<%>) with its center at
;    the given point and radius as given.
(define (create-circle pos r)
  (new Circle% 
       [x (posn-x pos)] 
       [y (posn-y pos)] 
       [radius r]
       [state creating]))

; create-rectangle : BoundingBox -> Shape<%>
; GIVEN: A bounding box,
; RETURNS: A new Rectangle% object (implementing Shape<%>) which is bounded
;    by the given BoundingBox.
(define (create-rectangle b)
  (new Rectangle% 
       [left (first b)] 
       [top (first b)]
       [right (first b)]
       [bottom (first b)] 
       [state creating]
       [start (make-posn 0 0)]))

(define (init-rectangle c1 c2)
  (new Rectangle% 
       [left 0] 
       [top 0]
       [right 0] 
       [bottom 0] 
       [state creating]
       [start (make-posn c1 c2)]))

