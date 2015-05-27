;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-unavail) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")
(require rackunit)
(require racket/set)
(require "sched-general.rkt")

(provide schedule/unavail-ok?)
(provide schedule/unavail)

; schedule/unavail-ok? : StudentUnavails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; Strategy: Function composition
(begin-for-test
  (check-true (schedule/unavail-ok? 
               STUS (list
                     (make-codewalk 't2 (list 'sud) 1)
                     (make-codewalk 't3 (list 'han 'xuan) 3)
                     (make-codewalk 't1 (list 'pai) 2)
                     (make-codewalk 't2 empty 0)
                     (make-codewalk 't4 empty 0)))))
(define (schedule/unavail-ok? students cws)
  (check-for-avail/unavail-ok? students cws false))

; schedule/unavail : StudentUnavails CodeWalks -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Strategy: Function composition
(begin-for-test
  (check-equal? (schedule/unavail STUS CWS)
                (list
                 (make-codewalk 't2 (list 'sud) 1)
                 (make-codewalk 't3 (list 'han 'xuan) 3)
                 (make-codewalk 't1 (list 'pai) 2)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't4 empty 0))
                "Test failed. schedule/unavail function should return a 
codewalk schedule"))
(define (schedule/unavail students cws)
  (check-for-avail/unavail students cws false))
