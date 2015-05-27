;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(require "extras.rkt")

(require rackunit)

(require 2htdp/image)

(require 2htdp/universe)

(define TIME-ON-TASK 4.5) ; hours




(provide edit)

(provide string->editor)

(provide editor-pre)

(provide editor-post)

(provide editor-pos)


;(check-location "02" "fsm.rkt")



; constants __________________________________

; graphcial constants:

(define BKG (empty-scene 200 20))                    ; background empty scene

(define CURSOR-IMAGE (rectangle 1 20 "solid" "red")) ; cursor's image

(define BLACK "black")  ; text color

(define FONT-SIZE 16)   ; text font size

(define MAX-LENGTH 20)  ; text max length on canvas

(define EMPTY '())      ; constant empty, list







; data definition __________________________________

; A 1String is a String whose lenght is always 1
; data example
(define char1 "a")  ; 1String
(define char2 " ")  ; 1String
(define char3 "\b") ; 1String


; An Lo1S is one of:
; – empty
; – (cons 1String Lo1S)
; data example
(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))


;; TEMPLATE:
;lo1s-fn : Lo1S -> ???
;(define (lo1s-fn lst)
;  (cond
;    [(empty? lst) ...]
;    [else (... (first lst)
;               (lo1s-fn (rest lst)))]))





(define-struct editor [prec postc])
; An Editor is (make-editor Lo1S Lo1S)
; INTERP: pre represents the text befor cursor,
;         while post represents the text after cursor
; data example
(define editor1 (make-editor lla good))


; TEMPLATE:
; editor-fn : Editor -> ???
;(define (editor-fn ed)
;  (... (editor-prec ed) ... (editor-postc ed) ...))




; A World is an Editor
; INTERP: Represents the current state of the editor
(define INITIAL-WORLD (make-editor EMPTY EMPTY))











; World functions ————————————————————

; run : World -> World
; Computes the next state of the World
; STRATEGY: function composition
(define (run w)
  (big-bang w
            (to-draw render)
            (on-key edit)))


; render : World -> Image
; Renders the current World state into a Image
(begin-for-test
  (check-equal? (render INITIAL-WORLD)
                (overlay/align "left" "center"
                (beside (text (editor-pre INITIAL-WORLD) FONT-SIZE BLACK)
                        CURSOR-IMAGE
                        (text (editor-post INITIAL-WORLD) FONT-SIZE BLACK))
                 BKG)
                "render the INITIAL-WORLD"))
; STRATEGY: function composition
(define (render w)
  (editor->image w))


; edit : World KeyEvent -> World
; Computes the next World after one key is pressed
(begin-for-test
  (check-equal? (edit (make-editor EMPTY EMPTY) "\t")
                (make-editor EMPTY EMPTY)
                "'\t' pressed, do nothing")
  (check-equal? (edit (make-editor EMPTY EMPTY) "\r")
                (make-editor EMPTY EMPTY)
                "'\r' pressed, do nothing")
  (check-equal? (edit (make-editor EMPTY EMPTY) "\b")
                (make-editor EMPTY EMPTY)
                "'\b' pressed, but cursor is at head, do nothing")
  (check-equal? (edit (make-editor (cons 1 '()) EMPTY) "\b")
                (make-editor EMPTY EMPTY)
                "'\b' pressed, delete '1'")
  (check-equal? (edit (make-editor EMPTY EMPTY) "5")
                (make-editor (cons "5" '()) EMPTY)
                "'5' pressed, add it to the tail")
  (check-equal? (edit (make-editor (cons 1 '()) (cons 2 '())) "left")
                (make-editor EMPTY (append (cons 1 '()) (cons 2 '())))
                "left pressed, move cursor before '1'")
  (check-equal? (edit (make-editor (cons 1 '()) (cons 2 '())) "right")
                (make-editor (append (cons 2 '()) (cons 1 '())) EMPTY)
                "right pressed, move cursor after '2'")
  (check-equal? (edit (make-editor EMPTY EMPTY) "down")
                (make-editor EMPTY EMPTY)
                "down pressed, do nothing"))
; STRATEGY: function composition
(define (edit ed ke)
  (edit-editor ed ke))












; Editor Functions -------------------

; edit-editor : Editor KeyEvent -> Editor
; Returns a new Editor after 
; - adding a char
; - deleting a char
; - moving cursor left or right to the current Editor
(begin-for-test
  (check-equal? (edit-editor INITIAL-WORLD "\t")
                INITIAL-WORLD
                "do nothing")
  (check-equal? (edit-editor INITIAL-WORLD "\r")
                INITIAL-WORLD
                "do nothing")
  (check-equal? (edit-editor INITIAL-WORLD "left")
                INITIAL-WORLD
                "cursor move left, but the left part is empty, do nothing"))
; STRATAGY: data decomposition on KeyEvent : ke
(define (edit-editor ed ke)
  (cond
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(string=? "\b" ke) (delete-char ed)]
    [(= (string-length ke) 1) (add-char ed ke)]
    [(string=? "left" ke) (cursor-left ed)]
    [(string=? "right" ke) (cursor-right ed)]
    [else ed]))


; editor->image : Editor -> Image
; Renders the current Editor into Image
(begin-for-test
  (check-equal? (editor->image INITIAL-WORLD)
                (overlay/align "left" "center"
                 (beside (text (editor-pre INITIAL-WORLD) FONT-SIZE BLACK)
                         CURSOR-IMAGE
                         (text (editor-post INITIAL-WORLD) FONT-SIZE BLACK))
                 BKG)
                "render the initial editor"))
; STRATEGY: function composition
(define (editor->image ed)
  (overlay/align "left" "center"
                 (beside (text (editor-pre ed) FONT-SIZE BLACK)
                         CURSOR-IMAGE
                         (text (editor-post ed) FONT-SIZE BLACK))
                 BKG))


; string->editor : String -> Editor
; Returns an Editor containing text str and cursor at position 0.
(begin-for-test
  (check-equal? (string->editor "123")
                (make-editor EMPTY (explode "123"))
                "An editor contains '123' with cursor at position 0
                 is a (make-editor '123' 0)")
  (check-equal? (string->editor "")
                (make-editor EMPTY (explode ""))
                "An editor contains empty text with cursor at position 0
                 is a (make-editor '' 0)")
  (check-equal? (string->editor " ")
                (make-editor EMPTY (explode " "))
                "An editor contains one space with cursor at position 0
                 is a (make-editor ' ' 0)"))
; STRATEGY: function composition
(define (string->editor str)
  (make-editor EMPTY (explode str)))


; editor-pre : Editor -> String
; Returns the text in editor e before the cursor.
(begin-for-test
  (check-equal? (editor-pre (make-editor EMPTY EMPTY)) 
                ""
                "the pre-cursor part of empty string is still empty")
  (check-equal? (editor-pre (make-editor (cons "a" EMPTY) EMPTY)) 
                "a"
                "the pre-cursor part of empty string is still empty"))
; STRATEGY: data decomposition on Editor : e
(define (editor-pre e)
  (if (empty? (editor-prec e))
      ""
      (implode (reverse (editor-prec e)))))



; editor-post : Editor -> String
; Returns the text in editor e after the cursor.
(begin-for-test
  (check-equal? (editor-post (make-editor EMPTY EMPTY))
                ""
                "the text in editor (make-editor '' 0) after the cursor is ''")
  (check-equal? (editor-post (make-editor EMPTY (cons "a" EMPTY)))
                "a"
                "the text in editor (make-editor '1234' 0) after the cursor is '1234'"))
; STRATEGY: data decomposition on Editor : e
(define (editor-post e)
  (if (empty? (editor-postc e))
      ""
      (implode (editor-postc e))))


; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor e.
(begin-for-test
  (check-equal? (editor-pos (make-editor EMPTY EMPTY))
                0
                "the position of the cursor in editor (make-editor 'pdp' 2) is 2"))
; STRATEGY: data decomposition on Editor : e
(define (editor-pos e)
  (length (editor-prec e)))


; add-char : Editor 1String -> Editor
; Return a new Editor after adding a char to the current Editor
(begin-for-test
  (check-equal? (add-char (make-editor EMPTY EMPTY) "a")
                (make-editor (cons "a" EMPTY) EMPTY)
                "add 'a' to the head of the text")
  (check-equal? (add-char (make-editor (explode "12345678901234567890") EMPTY) "a")
                (make-editor (explode "12345678901234567890") EMPTY)
                "'a' pressed, but text length is MAX-LENGTH, cannot add char any more"))
; STRATEGY: data decomposition on Editor : ed
(define (add-char ed c)
  (if (= (text-length ed)  MAX-LENGTH)
      ed
      (make-editor (cons c (editor-prec ed))
                   (editor-postc ed))))



; text-length : Editor -> Natural
(begin-for-test
  (check-equal? (text-length INITIAL-WORLD)
                0
                "initial world length is zero"))
; STRATEGY: data decomposition on Editor : ed
(define (text-length ed)
  (+ (length (editor-prec ed)) (length (editor-postc ed))))



; delete-char : Editor -> Editor
; Return a new Editor after deleting a char to the current Editor
(begin-for-test
  (check-equal? (delete-char INITIAL-WORLD)
                INITIAL-WORLD
                "nothing to remove from the inital editor"))
; STRATEGY: data decomposition on Editor : ed
(define (delete-char ed)
  (if (empty? (editor-prec ed))
      ed
      (make-editor (delete-first (editor-prec ed))
                   (editor-postc ed))))


  

; cursor-left : Editor -> Editor
; Return a new Editor after moving the current cursor left once, if possible
(begin-for-test
  (check-equal? (cursor-left INITIAL-WORLD)
                INITIAL-WORLD
                "cursor move left in the inital editor, do nothing"))
; STRATEGY: data decomposition on Editor : ed
(define (cursor-left ed)
  (left-char-exchange (editor-prec ed) (editor-postc ed)))
  

  
; cursor-right : Editor -> Editor
; Return a new Editor after moving the current cursor right once, if possible
(begin-for-test
  (check-equal? (cursor-right INITIAL-WORLD)
                INITIAL-WORLD
                "cursor move right in the inital editor, do nothing"))
; STRATEGY: data decomposition on Editor : ed
(define (cursor-right ed)
  (right-char-exchange (editor-prec ed) (editor-postc ed)))
  
 









  
; Lo1S Functions __________________________
  
; delete-fisrt : Lo1S 1String -> Lo1S
; delete first element of the given list l
; WHERE : list l is not a empty list
(begin-for-test
  (check-equal? (delete-first (make-list 3 "a"))
                (make-list 2 "a")
                "remove first a"))
; STRATEGY: function composition
(define (delete-first l)
  (remove (first l) l))  

  
  
; left-char-exchange : Lo1S Lo1S -> Editor
; move the first element of list pre to list post,
; then assemble them as a new editor
(begin-for-test
  (check-equal? (left-char-exchange (make-list 1 "a") (make-list 1 "b"))
                (make-editor EMPTY (cons "a" (cons "b" '())))  
                ""))
; STRATEGY: function composition
(define (left-char-exchange pre post)
  (if (empty? pre)
      (make-editor pre post)
      (make-editor (delete-first pre)
                   (cons (first pre) post))))




; right-char-exchange : Lo1S Lo1S -> Editor
; move the first element of list post to list pre,
; then assemble them as a new editor
(begin-for-test
  (check-equal? (right-char-exchange (make-list 1 "a") (make-list 1 "b"))
                (make-editor (cons "b" (cons "a" '())) EMPTY)  
                ""))
; STRATEGY: function composition
(define (right-char-exchange pre post)
  (if (empty? post)
      (make-editor pre post)
      (make-editor (cons (first post) pre)
                       (delete-first post))))










; Main Function ———————————————————

;(run INITIAL-WORLD)











; ================= Alternate Data Definition =================



;1. Compare with the two-string data definition

; Pros: 
;  - easier to extend the program by using large amount of build-in functions 




; Cons:
;  - when it comes to return the left part of the editor, the two string version
;    is much easier than the two list version

;    function editor-pre is a build-in funciton


; INSTEAD OF:

;   (define (editor-pre e)
;      (if (empty? (editor-prec e))
;          ""
;          (implode (reverse (editor-prec e)))))














;2. Compare with the string plus index data definition

; Pros:
;  - By using build-in function, add char into the text
;   
;    

;    (define (add-char ed c)
;       (if (= (text-length ed)  MAX-LENGTH)
;       ed
;       (make-editor (cons c (editor-prec ed))
;                    (editor-postc ed))))

; INSTEAD OF:

;     (define (add-char ed c)
;        (if (= (string-length (editor-text ed)) MAX-LENGTH)
;            ed
;            (make-editor (string-append (editor-pre ed) c (editor-post ed))
;                         (add1 (editor-pos ed)))))

;  - large amount of build-in fucntions like : implode, explode, reverse etc. makes it easier to handl
;    list



; Cons:
;      - it's more difficult to move the cursor. With the string plus index data definition
;        we can just change the index. But with the two lists definition, we have to define 
;        helper functions to complete this task.

;        string plus index version:
;        (define (cursor-left ed)
;           (make-editor (editor-text ed)
;                        (max 0 (sub1 (editor-pos ed)))))

; INSTEAD OF:

;        two list version:
;        (define (cursor-right ed)
;           (right-char-exchange (editor-prec ed) (editor-postc ed)))

;        (define (right-char-exchange pre post)
;           (if (empty? post)
;               (make-editor pre post)
;               (make-editor (cons (first post) pre)
;                            (delete-first post))))


;      - complex to read and understand