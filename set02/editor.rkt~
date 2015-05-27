;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 6) ; hours


;(check-location "02" "fsm.rkt")


; graphcial constants:
(define BKG (empty-scene 200 20)) ; background empty scene
(define CURSOR-IMAGE (rectangle 1 20 "solid" "red")) ; cursor's image
(define BLACK "black")  ; text color
(define FONT-SIZE 16)   ; text font size
(define MAX-LENGTH 20)  ; text max length on canvas


; A 1String is a String whose lenght is always 1
(define char1 "a")  ; 1String
(define char2 " ")  ; 1String
(define char3 "\b") ; 1String


; Editor definition ———————————————————
; Editor = (make-editor String Natural)
; INTERP: text means the content in the editor,
;         cindex represents the current position of the cursor, 
;         (how many characters befor cursor, start from 0)
(define-struct editor [text cindex])

; TEMPLATE:
; editor-fn : Editor -> ???
(define (editor-fn ed)
  (... (editor-text ed) ... (editor-cindex ed) ...))


; World definition ———————————————————
; A World is an Editor
; INTERP: Represents the current state of the editor
(define INITIAL-WORLD (make-editor "" 0))


; World functions ————————————————————

; run : World -> World
; Computes the next state of the World
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
(define (render w)
  (editor->image w))


; edit : World KeyEvent -> World
; Computes the next World after one key is pressed
(begin-for-test
  (check-equal? (edit (make-editor "1234" 0) "\t")
                (make-editor "1234" 0)
                "'\t' pressed, do nothing")
  (check-equal? (edit (make-editor "1234" 0) "\r")
                (make-editor "1234" 0)
                "'\r' pressed, do nothing")
  (check-equal? (edit (make-editor "1234" 0) "\b")
                (make-editor "1234" 0)
                "'\b' pressed, but cursor is at head, do nothing")
  (check-equal? (edit (make-editor "1234" 1) "\b")
                (make-editor "234" 0)
                "'\b' pressed, delete '1'")
  (check-equal? (edit (make-editor "1234" 4) "5")
                (make-editor "12345" 5)
                "'5' pressed, add it to the tail")
  (check-equal? (edit (make-editor "1234" 4) "left")
                (make-editor "1234" 3)
                "left pressed, move cursor after '3'")
  (check-equal? (edit (make-editor "1234" 0) "right")
                (make-editor "1234" 1)
                "right pressed, move cursor after '1'")
  (check-equal? (edit (make-editor "1234" 2) "down")
                (make-editor "1234" 2)
                "down pressed, do nothing"))
(define (edit ed ke)
  (edit-editor ed ke))



; Editor Functions -------------------

; edit-editor : Editor KeyEvent -> Editor
; Returns a new Editor after 
; - adding a char
; - deleting a char
; - moving cursor left or right to the current Editor
; STRATAGY: data decomposition
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
                (make-editor "123" 0)
                "An editor contains '123' with cursor at position 0
                 is a (make-editor '123' 0)")
  (check-equal? (string->editor "")
                (make-editor "" 0)
                "An editor contains empty text with cursor at position 0
                 is a (make-editor '' 0)")
  (check-equal? (string->editor " ")
                (make-editor " " 0)
                "An editor contains one space with cursor at position 0
                 is a (make-editor ' ' 0)"))
(define (string->editor str)
  (make-editor str 0))


; editor-pre : Editor -> String
; Returns the text in editor e before the cursor.
(begin-for-test
  (check-equal? (editor-pre (make-editor "" 0)) 
                ""
                "the pre-cursor part of empty string is still empty")
  (check-equal? (editor-pre (make-editor "pdp" 3)) 
                "pdp"
                "the pre-cursor part of 'pdp' is 'pdp', where cursor index is 3")
  (check-equal? (editor-pre (make-editor "     " 3)) 
                "   "
                "the pre-cursor part of '     ' is '   ', where cursor index is 3"))
(define (editor-pre e)
  (substring (editor-text e) 0 (editor-cindex e)))


; editor-post : Editor -> String
; Returns the text in editor e after the cursor.
(begin-for-test
  (check-equal? (editor-post (make-editor "" 0))
                ""
                "the text in editor (make-editor '' 0) after the cursor is ''")
  (check-equal? (editor-post (make-editor "1234" 0))
                "1234"
                "the text in editor (make-editor '1234' 0) after the cursor is '1234'")
  (check-equal? (editor-post (make-editor "1234" 4))
                ""
                "the text in editor (make-editor '1234' 4) after the cursor is ''")
  (check-equal? (editor-post (make-editor " " 0))
                " "
                "the text in editor (make-editor ' ' 0) after the cursor is ' '"))
(define (editor-post e)
  (substring (editor-text e) 
             (editor-cindex e)
             (string-length (editor-text e))))


; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor e.
(begin-for-test
  (check-equal? (editor-pos (make-editor "pdp" 2))
                2
                "the position of the cursor in editor (make-editor 'pdp' 2) is 2"))
(define (editor-pos e)
  (editor-cindex e))


; add-char : Editor 1String -> Editor
; Return a new Editor after adding a char to the current Editor
(begin-for-test
  (check-equal? (add-char (make-editor "1234" 0) "a")
                (make-editor "a1234" 1)
                "add 'a' to the head of the text")
  (check-equal? (add-char (make-editor "12345678901234567890" 0) "a")
                (make-editor "12345678901234567890" 0)
                "'a' pressed, but text length is MAX-LENGTH, cannot add char any more"))
(define (add-char ed c)
  (if (= (string-length (editor-text ed)) MAX-LENGTH)
      ed
      (make-editor (string-append (editor-pre ed) c (editor-post ed))
                   (add1 (editor-pos ed)))))

; delete-char : Editor -> Editor
; Return a new Editor after deleting a char to the current Editor
(begin-for-test
  (check-equal? (delete-char (make-editor "1234" 0))
                (make-editor "1234" 0)
                "delete char before postion 0, do nothing")
  (check-equal? (delete-char (make-editor "1234" 4))
                (make-editor "123" 3)
                "delete char at tail, delete '4'")
  (check-equal? (delete-char (make-editor "" 0))
                (make-editor "" 0)
                "delete char from empty text, do nothing")
  (check-equal? (delete-char (make-editor " " 1))
                (make-editor "" 0)
                "delete a space"))
(define (delete-char ed)
  (if (= (string-length (editor-pre ed)) 0)
      ed
      (make-editor (string-append (substring (editor-pre ed)
                                             0
                                             (- (editor-pos ed) 1))
                                  (editor-post ed))
                   (sub1 (editor-pos ed)))))


; cursor-left : Editor -> Editor
; Return a new Editor after moving the current cursor left once, if possible
(begin-for-test
  (check-equal? (cursor-left (make-editor "" 0))
                (make-editor "" 0)
                "move cursor in empty text")
  (check-equal? (cursor-left (make-editor "1234" 0))
                (make-editor "1234" 0)
                "cannot move cursor toward left, cursor already at head")
  (check-equal? (cursor-left (make-editor "1234" 4))
                (make-editor "1234" 3)
                "move cursor toward left once, cursor now after 4"))
(define (cursor-left ed)
  (make-editor (editor-text ed)
               (max 0 (sub1 (editor-pos ed)))))
  
  
; cursor-right : Editor -> Editor
; Return a new Editor after moving the current cursor right once, if possible
(begin-for-test
  (check-equal? (cursor-right (make-editor "" 0))
                (make-editor "" 0)
                "move cursor in empty text")
  (check-equal? (cursor-right (make-editor "1234" 0))
                (make-editor "1234" 1)
                "move cursor toward right once, cursor now after '1'")
  (check-equal? (cursor-right (make-editor "1234" 4))
                (make-editor "1234" 4)
                "cannot move cursor toward right, cursor already at tail"))
(define (cursor-right ed)
  (make-editor (editor-text ed)
               (min (string-length (editor-text ed))
                    (add1 (editor-pos ed)))))
  
 

; Main Function ———————————————————

(run INITIAL-WORLD)




