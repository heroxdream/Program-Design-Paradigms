;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 3.5) ; hours

(provide INITIAL-WORLD)
(provide next-state)
(provide render)
(provide stop?)
(provide accept-state?)
(provide error-state?)

; constants: 
(define LENGTH 100)      ; pixels

;(check-location "02" "fsm.rkt")



; Pixels definition ———————————————————
; A Pixels is a NonNegReal, representing a Image distance.

; Color definition ———————————————————
; A Color is a String, representing the color of the Image
(define WHITE  "white")  ; color
(define YELLOW "yellow") ; color
(define GREEN  "green")  ; color
(define RED    "red")    ; color

; ExpectsToSee definition ———————————————————
; An ExpectsToSee is one of: 
; – AA
; – BC
; – DD 
; – ER 

(define AA "start, expect to see an 'a' next")
(define BC "expect to see: 'b', 'c', or 'd'")
(define DD "encountered a 'd', finished")
(define ER "error, user pressed illegal key")

; TEMPLATE:
; expectstosee-fn : Editor -> ???
;(define (expectstosee-fn e)
;  (cond
;    [(AA? e) ...]
;    [(BC? e) ...]
;    [(DD? e) ...]
;    [(ER? e) ...]))


; World definition ———————————————————
; A World is An ExpectsToSee
; INTERP: representing the current state of the world
(define INITIAL-WORLD AA)


; World functions ————————————————————

; run : World -> World
; Recognizes a pattern in a sequence of KeyEvents and translate them into Image
; initial state is a white image
; press 'a' it turns yellow, other keys turns red
; then press 'b' or 'c' it stays in yellow, 'd' turns green, dother keys turns red
(define (run w)
  (big-bang w
            (on-key next-state)
            (to-draw render)
            (stop-when stop?)))


; next-state : World KeyEvent -> World
; Computes next world state reponse to current world state and KeyEvent
(begin-for-test
  (check-equal? (next-state AA "a")
                BC
                "current state AA, press 'a', turns to BC")
  (check-equal? (next-state AA "b")
                ER
                "current state AA, press 'b', turns to ER")
  (check-equal? (next-state BC "b")
                BC
                "current state BC, press 'b', stay unchanged")
  (check-equal? (next-state BC "c")
                BC
                "current state BC, press 'c', stay unchanged")
  (check-equal? (next-state BC "d")
                DD
                "current state BC, press 'd', turns to DD")
  (check-equal? (next-state BC "a")
                ER
                "current state BC, press 'd', turns to ER"))
(define (next-state w k)
  (next-ETS w k))


; render : World -> Image
; render the current World state
(begin-for-test
  (check-equal? (render AA)
                (rectangle LENGTH LENGTH "solid" WHITE)
                "render World state AA")
  (check-equal? (render BC)
                (rectangle LENGTH LENGTH "solid" YELLOW)
                "render World state BC")
  (check-equal? (render DD)
                (rectangle LENGTH LENGTH "solid" GREEN)
                "render World state DD")
  (check-equal? (render ER)
                (rectangle LENGTH LENGTH "solid" RED)
                "render World state ER"))
(define (render w)
  (ETS->image w))

; stop? : World -> Boolean
; Returns true if the current state is ER(error state) or DD(final/accept state)
(begin-for-test
  (check-true (stop? DD) "stop at DD(final/accept state, 'd' pressed)")
  (check-true (stop? ER) "stop at ER(error state, illegal key pressed)")
  (check-false (stop? AA) "do not stop at AA, just start")
  (check-false (stop? BC) "do not stop at BC, legal key"))
(define (stop? w)
  (or (accept-state? w) (error-state? w)))



; ExpectsToSee functions ————————————————————

; AA? : ExpectsToSee -> Boolean
; Return true if the ExpectsToSee e is AA
(begin-for-test
  (check-true (AA? AA)
              "true")
  (check-false (AA? DD)
              "false"))
(define (AA? e)
  (string=? AA e))

; BC? : ExpectsToSee -> Boolean
; Return true if the ExpectsToSee e is BC
(begin-for-test
  (check-true (BC? BC)
              "true")
  (check-false (BC? DD)
              "false"))
(define (BC? e)
  (string=? BC e))

; DD? : ExpectsToSee -> Boolean
; Return true if the ExpectsToSee e is DD
(begin-for-test
  (check-true (DD? DD)
              "true")
  (check-false (DD? BC)
              "false"))
(define (DD? e)
  (string=? DD e))

; ER? : ExpectsToSee -> Boolean
; Return true if the ExpectsToSee e is ER
(begin-for-test
  (check-true (ER? ER)
              "true")
  (check-false (ER? AA)
              "false"))
(define (ER? e)
  (string=? ER e))

; next-ETS : ExpectsToSee -> ExpectsToSee
; Computes next ExpectsToSee
; STRATAGY: data decomposition
(define (next-ETS ets k)
  (cond
    [(and (AA? ets) (string=? "a" k)) BC]
    [(and (BC? ets) (string=? "b" k)) BC]
    [(and (BC? ets) (string=? "c" k)) BC]
    [(and (BC? ets) (string=? "d" k)) DD]
    [else ER]))


; ETS->image : ExpectsToSee -> Image
; Renders the current ExpectsToSee into Image
; STRATAGY: data decomposition
(define (ETS->image ets)
  (cond
    [(AA? ets) (show-image WHITE)]
    [(BC? ets) (show-image YELLOW)]
    [(DD? ets) (show-image GREEN)]
    [(ER? ets) (show-image RED)]))


; accept-state? : ExpectsToSee -> Boolean
; Returns true if ExpectsToSee e is an accepting state.
(begin-for-test
  (check-true (accept-state? DD)
              "DD is the final/accept state")
  (check-false (accept-state? AA)
              "AA is not the final/accept state")
  (check-false (accept-state? BC)
              "BC is not the final/accept state")
  (check-false (accept-state? ER)
              "ER is not the final/accept state"))
(define (accept-state? e)
  (DD? e))

; error-state? : ExpectsToSee -> Boolean
; Returns true if ExpectsToSee e is an error state.
(begin-for-test
  (check-true (error-state? ER)
              "ER is the error state")
  (check-false (error-state? AA)
              "AA is not the error state")
  (check-false (error-state? BC)
              "BC is not the error state")
  (check-false (error-state? DD)
              "DD is not the error state"))
(define (error-state? e)
  (ER? e))


; Helper Functions ———————————————————

; show-image : Color -> Image
; show a 100 * 100 square with regard the given color
(begin-for-test
  (check-equal? (show-image WHITE)
                (rectangle LENGTH LENGTH "solid" WHITE)
                "show a 100 * 100 white square")
  (check-equal? (show-image YELLOW)
                (rectangle LENGTH LENGTH "solid" YELLOW)
                "show a 100 * 100 yellow square")
  (check-equal? (show-image GREEN)
                (rectangle LENGTH LENGTH "solid" GREEN)
                "show a 100 * 100 green square")
  (check-equal? (show-image RED)
                (rectangle LENGTH LENGTH "solid" RED)
                "show a 100 * 100 red square"))
(define (show-image c)
  (rectangle LENGTH LENGTH "solid" c))



; Main Function ———————————————————

(run INITIAL-WORLD)


