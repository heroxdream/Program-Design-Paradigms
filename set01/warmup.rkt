;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(define TIME-ON-TASK 7) ; hours


;(check-location "01" "warmup.rkt")

;Exercise 13 >>>>>>>>>>>>>>>>>>>>

; Coordnite definition -------
; A Coordnite is a Real, representing the x or y postion of the point

; Diatance definition -------
; A Distance is a NonNegReal, representing distance from origin

(define X 3)        ;Coordinate
(define Y 4)        ;Coordinate
(define ORIGIN-X 0) ;Coordinate
(define ORIGIN-Y 0) ;Coordinate

; distance-to-origin : Coordinate Coordinate -> Distance
; computes the gemotrical distance between the given Coordnites and Origin
(begin-for-test
  (check-equal? (distance-to-origin ORIGIN-X ORIGIN-Y) 
               0 
               "zero if the given points is the origin")
  (check-equal? (distance-to-origin X Y)
               (sqrt(+ (sqr X) (sqr Y)))
               "given point is not origin, compute then"))
; STRATEGY: function composition
(define (distance-to-origin x y)
  (sqrt(+ (sqr x) (sqr y))))



;Exercise 14 >>>>>>>>>>>>>>>>>>>>

; Length definition -------
; A Lenght is a NonNegReal, representing side length of a given cube
(define LEN1 0) ; pixels
(define LEN2 2) ; pixels

; Volume definition -------
; A Volume is a Real, represents the volume of the cube
(define VOLUME1 0) ; pixels^3
(define VOLUME2 8) ; pixels^3

; Surface definition -------
; A Surface is a Real representing the surface of the cube
(define SURFACE1 0)    ; pixels^2
(define SURFACE2 24)   ; pixels^2

; cube-volume : Length -> Volume
; Computes the volume of the cube
(begin-for-test
  (check-equal? (cube-volume LEN1) 
                VOLUME1 
                "return 0 if length is 0")
  (check-equal? (cube-volume LEN2) 
                VOLUME2 
                "positive length"))
; STRATEGY: function composition
(define (cube-volume length)
  (* length length length))

; cube-surface : Length -> Surface
; Computes the surface of the cube
(begin-for-test
  (check-equal? (cube-surface LEN1) 
                SURFACE1 
                "return 0 if length is 0")
  (check-equal? (cube-surface LEN2) 
                SURFACE2 
                "positive length"))
; STRATEGY: function composition
(define (cube-surface length) 
  (* 6 length length))



;Exercise 15 >>>>>>>>>>>>>>>>>>>>

; 1String definition -------
; A 1String is all string whose length is 1

(define STR1 "I like PDP")    ; start and end with normal character
(define STR2 " I like PDP ")  ; start and end with blank

; String -> 1String
; Extracts the first Character of the String
; WHERE: the input String str cannot be a empty string
(begin-for-test
  (check-equal? (string-first STR1)
                "I" 
                "not blank head")
  (check-equal? (string-first STR2)
                " " 
                "a blank head"))
; STRATEGY: function composition
(define (string-first str)
  (substring str 0 1))



;Exercise 16 >>>>>>>>>>>>>>>>>>>>

; string-last : String -> 1String
; Extracts the last Character of the String
; WHERE: the input String str cannot be a empty string
(begin-for-test
  (check-equal? (string-last STR1) 
                "P" 
                "not blank tail")
  (check-equal? (string-last STR2) 
                " " 
                "blank tail"))
; STRATEGY: function composition
(define (string-last str)
  (substring str (- (string-length str) 1)))



;Exercise 17 >>>>>>>>>>>>>>>>>>>>

(define T #true)   ; #true
(define F #false)  ; #false

; bool-imply : Boolean Boolean -> Boolean
; return #true if b1 is false or b2 is true
(begin-for-test
  (check-false (bool-imply T F) "false case")
  (check-true  (bool-imply T T) "true case")
  (check-true  (bool-imply F T) "true case")
  (check-true  (bool-imply F F) "true case"))
; STRATEGY: function composition
(define (bool-imply b1 b2)
  (or (not b1) b2))




;Exercise 18 >>>>>>>>>>>>>>>>>>>>

; Area definition -------
; A Area is a NonNegReal
; INTERP: the image is considered within a rectangle image,
;         so the Area (total pixels) of the image can be easily
;         counted by: image-height * image-width

(define IMG1 (rectangle 20 40 "solid" "black")) ; tall rectangle
(define IMG2 (circle 10 "solid" "black"))       ; circle
(define IMG3 (circle 0 "solid" "black"))        ; zero radius circle

; image-area : Image -> Area
; Computes the Area (total count of pixels) of a given image
; WHERE: the image is always within a regular rectangle image
(begin-for-test
  (check-equal? (image-area IMG1) 
                (* 20 40) 
                "rectangle area")
  (check-equal? (image-area IMG2) 
                (* (* 10 2) (* 10 2)) 
                "circle area")
  (check-equal? (image-area IMG3) 
                0 
                "zero radius circle"))
; STRATEGY: function compositions
(define (image-area img)
  (* (image-width img) (image-height img)))



;Exercise 19 >>>>>>>>>>>>>>>>>>>>

; A ImageClass is one of
; - "tall"
; - "wide"
; - "square"
; INTERP: "tall" if the image is taller than its width 
;         "wide" if the image is wider than its heigth
;         "square" if the image has the same width and height

(define TALL "tall")
(define WIDE "wide")
(define SQUARE "square")

(define IMG4 (rectangle 40 20 "solid" "red"))

; image-classify : Image -> ImageClass
; Produces "tall" if the image is taller than it is wide, 
;          "wide" if it is wider than it is tall, or
;          "square" if its width and height are the same.
(begin-for-test
  (check-equal? TALL 
                "tall" 
                "check for typo when creating constant")
  (check-equal? WIDE 
                "wide" 
                "check for typo when creating constant")
  (check-equal? SQUARE 
                "square" 
                "check for typo when creating constant")
  (check-equal? (image-classify IMG1) 
                TALL   
                "tall")
  (check-equal? (image-classify IMG2) 
                SQUARE 
                "square")
  (check-equal? (image-classify IMG3) 
                SQUARE 
                "height=width=0, square")
  (check-equal? (image-classify IMG4) 
                WIDE   
                "wide"))
; STRATEGY: data decomposition
(define (image-classify img)
  (cond
    [(> (image-height img) (image-width img)) TALL]
    [(< (image-height img) (image-width img)) WIDE]
    [(= (image-height img) (image-width img)) SQUARE]))



;Exercise 20 >>>>>>>>>>>>>>>>>>>>

(define STR3 " ")   ; a string contains only one blank
(define STR4 "PDP") ; normal string
(define STR5 "")    ; empty string

; string-join : String String -> String
; Concatenates two string by "_"
(begin-for-test
  (check-equal? (string-join STR3 STR4) 
                " _PDP" 
                "blank")
  (check-equal? (string-join STR3 STR5) 
                " _" 
                "empty string")
  (check-equal? (string-join STR4 STR5) 
                "PDP_" 
                "empty string"))
; STRATEGY: function composition
(define (string-join str1 str2)
  (string-append str1 "_" str2))


;Exercise 21 >>>>>>>>>>>>>>>>>>>>

(define STR6 "1234") ; a string of numbers

; string-insert : String Integer -> String
; Inserts "_" at the ith position of the given string
; WHERE: i is a number between 0 and the length of the given string (inclusive)
(begin-for-test
  (check-equal? (string-insert STR5 0) 
                "_" 
                "insert into an empty string")
  (check-equal? (string-insert STR6 0) 
                "_1234" 
                "at head")
  (check-equal? (string-insert STR6 3) 
                "123_4" 
                "at middle")
  (check-equal? (string-insert STR6 4) 
                "1234_" 
                "at tail"))
; STRATEGY: function composition
(define (string-insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))


;Exercise 22 >>>>>>>>>>>>>>>>>>>>

; string-delete : String Integer -> String
; Deletes the ith position from given string
; WHERE:  i is a number between 0 (inclusive) and the length of 
;         the given string (exclusive)
;         AND the given string cannot be a empty string
(begin-for-test
  (check-equal? (string-delete STR3 0) 
                ""    
                "delete blank")
  (check-equal? (string-delete STR6 0) 
                "234" 
                "delete head")
  (check-equal? (string-delete STR6 2) 
                "124" 
                "delete middle")
  (check-equal? (string-delete STR6 3) 
                "123" 
                "delete tail"))
; STRATEGY: function composition
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ 1 i))))

