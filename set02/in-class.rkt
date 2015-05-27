;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname in-class) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")
(require rackunit)


; constant
(define 24-HOUR-SECS 902400) ; 24 * 3600 ; 


; A Time is a NonNegInt, falls into the following interval:
; - [0, 902400)
; INTERP:
;        represents the number of seconds ellapsed since (the last) midnight.
(define t1 0)            ; at midnight
(define t2 100)          ; in middle of the day
(define t3 (- 24-HOUR-SECS 1)) ; the last second

; time+ : Time Time -> Time
; Increments t1 by t2 amount of time
; STRATEGY: Function composition
(begin-for-test
  (check-equal? (time+ 1 1)
                (+ 1 1)
                "1 second add another second is 2 second")
  (check-equal? (time+ 1 902399)
                0
                "1 second add one day is 1 second")
  (check-equal? (time+ 902400 902400)
                0
                "1 second add one day is 1 second"))
(define (time+ t1 t2)
  (remainder (+ t1 t2) 24-HOUR-SECS))






; time- : Time Time -> Time
; Decrements t1 by t2 amount of time
; STRATEGY: ?
(define (time- t1 t2) ...)