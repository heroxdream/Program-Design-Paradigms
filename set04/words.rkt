;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname words) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")

(require rackunit)

(define TIME-ON-TASK 3.5)



(provide arrangements)

(provide insert-everywhere/in-all-words)

(provide arrangement-main)


; constant

(define EMPTY-HEAD '())






; data definition _______________________

; A Word is either
; – '() or
; – (cons 1String Word)
(define w1 '())
(define w2 (cons "a" (cons "b" '())))

;; TEMPLATE:
;word-fn : w -> ???
;(define (word-fn w)
;  (cond
;    [(empty? s) ...]
;    [else (... (first s)
;               (word-fn (rest s)))]))




; A List-of-Word is either
; – (list '()) or
; – (cons Word List-of-Word)
(define low1 (list'()))
(define low2
  (cons (cons "a" (cons "b" '())) 
        (cons (cons "c" (cons "d" '()))
              '())))

;; TEMPLATE:
;low-fn : w -> ???
;(define (low-fn lw)
;  (cond
;    [(empty? lw) ...]
;    [else (... (first lw)
;               (low-fn (rest lw)))]))





; functions ________________________________

; Word -> List-of-words
; creates a list of all rearrangements of the letters in w (define (arrangements w)
(begin-for-test 
  (check-equal? (arrangements '("a" "b"))
                '(("a" "b") ("b" "a"))
                "arrangements of 'ab'"))
; STRATAGY: data decomposition on Word : w
(define (arrangements w) 
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))



; insert-everywhere/in-all-words : 1String List-of-Word -> List-of-Word
; insert a 1String into all positions of a whole bunch of word list, return a List-of-Word
(begin-for-test
  (check-equal? (insert-everywhere/in-all-words "a" (list (list  "b") (list  "c")))
                (list (list "a" "b")
                      (list "b" "a")
                      (list "a" "c") 
                      (list "c" "a"))
                "insert a into (list (list  'b') (list  'c'))"))
; STRATAGY: data decomposition on List-of-Word : low
(define (insert-everywhere/in-all-words char low) 
  (cond
    [(empty? low) '()]
    [else (append (insert char EMPTY-HEAD (first low))
                (insert-everywhere/in-all-words char (rest low)))]))



; insert : 1String Word Word -> List-of-Word
; Inserts a 1String into all positions of a give word w, return a List-of-Word
(begin-for-test
  (check-equal? (insert "a" EMPTY-HEAD (list "b"))
                (list (list "a" "b")
                      (list "b" "a"))
                "insert 'a' into 'b'"))
; STRATAGY: data decomposition on Word : w
(define (insert char head w)
  (cond
    [(empty? w) (list (append head (list char)))]
    [else (cons (append head (list char) w) 
                (insert char 
                        (append head (list (first w)))
                        (rest w)))]))




; arrangement-main : String -> List-of-Word
; consumes a String and produces all of its possible permutation
; as a list of Strings
(begin-for-test
  (check-equal? (arrangement-main "ab")
                (list (list "a" "b")
                      (list "b" "a"))
                "permutation of string 'ab'"))
; STRATEGY: function composition
(define (arrangement-main str)
  (arrangements (explode str)))





; Main Function ____________________

;(arrangement-main "ab")










; ================= Alternate Data Definition =================


; 1. alternative data definition 1

; A Word is a String
; (define w1 "abcd")

; List-of-Word remains the same

; Pros:
;    - easier structor, easier to read and understand

; [(empty? w) (list (append head (list char)))]

; INSTEAD OF

; [(empty? w) (list (string-append head char))]



; Cons:
;    - harder to write resursive functions, because it's harder to deal with string
;      the build-in function for list is more convient.

;(define (word-fn w)
;  (cond
;    [(= 0 (length s) ...]
;    [else (... (string-ith w 1)
;               ... (word-fn (substring s))...)]))


; INSTEAD OF

;(define (word-fn w)
;  (cond
;    [(empty? s) ...]
;    [else (... (first s)
;               (word-fn (rest s)))]))



; 1. alternative data definition 2

; A Word is a NonNegInt
; INTERP: use 1234 represents "abcd", because 1 is the index of "a" in order,
;         and 2 is for "b" and so on.
; (define w1 1234)


; Pros:
;  - maybe we can find some mathmatical method to solve this problem



; Cons:
;  - hard to understand and read



