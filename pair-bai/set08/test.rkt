;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))


(define (valid-username? str)
  (and (< 1 (string-length str) 12)
       (only-letters-numbers str)))


(define (only-letters-numbers str)
  (andmap (lambda (char) (or (char-numeric? char)
                              (char-alphabetic? char)))
          (string->list str)))