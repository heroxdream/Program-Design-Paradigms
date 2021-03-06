;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 15) ;hours


(provide render)
(provide edit)
(provide string->editor)
(provide editor-pre)
(provide editor-post)
(provide editor-pos)

;global constants -----------------------------------
;RECTANGLE is a rectangle, which represents the cursor in the editor
(define RECTANGLE (rectangle 1 20 "solid" "red"))
;WIDTH represents the width of the scene
(define WIDTH 200)
;HEIGHT represents the height of the scene
(define HEIGHT 20)
;MTSCN represents an empty-scene 
(define MTSCN (empty-scene WIDTH HEIGHT))
;START-INDEX represents the first position of a string
(define START-INDEX 0)
;FIRST-INDEX represents the second position of a string
(define FIRST-INDEX 1)
;TEXT-SIZE represents the size of the text displayed in the text editor
(define TEXT-SIZE 16)
;TEXT-COLOR represents the color of the text displayed in the text editor
(define TEXT-COLOR "black")

;Editor Structure Definition --------------------
;Editor is a structure of the type (make-editor String String),
;INTERP :which means the text in the editor is (string-append s t) with the
; cursor displayed between s and t
(define-struct editor [pre post])
; TEMPLATE:
; editor-fn : Editor -> ???
;(define (editor-fn e)
;  (... (editor-pre e) ... (editor-post e) ...))


;Editor functions ----------------------------------
;print-pre : Editor -> Text
;Returns Text that contains the pre part of the Editor
(begin-for-test
  (check-equal? (print-pre (make-editor "hi" "there"))
                (text "hi" 16 "black")
                "Prints Text containing 'hi' "))
;STRATEGY : Data Decomposition on ed : Editor
(define (print-pre ed)
  (text (editor-pre ed) TEXT-SIZE TEXT-COLOR))

;print-post : Editor-> Text 
;Returns Text that contains the post part of the Editor
(begin-for-test
  (check-equal? (print-post (make-editor "hi" "there"))
                (text "there" 16 "black")
                "Prints Text containing 'there' "))
;STRATEGY : Data Decomposition on ed : Editor
(define (print-post ed)
  (text (editor-post ed) TEXT-SIZE TEXT-COLOR))

;render : Editor -> Image
;Returns an Image that contains a cursor between the pre and post parts 
; of the given Editor,
;on a empty-scene background
(begin-for-test
  (check-equal? (render (make-editor "hi" "there"))
                (overlay/align "left" "center" 
                (beside (text "hi" 16 "black") 
                        RECTANGLE (text "there" 16 "black")) MTSCN)
                "Draws an Image containg 'hi' 'there' with cursor in between' "))
;STRATEGY : Function Composition
(define (render ed) 
  (overlay/align "left" "center"
  (beside (print-pre ed) RECTANGLE (print-post ed))
   MTSCN))

;Key is a single-character String, WHERE Key is not "\b", "\t", "\u007F" or "\r"
;Pre-String is a String, which represents the pre part of the Editor
;pre-ke-appender : Editor Key -> Pre-String
;Appends the given Key to the pre part of the given Editor
(begin-for-test
  (check-equal? (pre-ke-appender (make-editor "hi" "there") "a")
                "hia"
                "Appends 'a' to 'hi'"))
;STRATEGY : Data Decomposition on ed : Editor
(define (pre-ke-appender ed ke)
  (string-append (editor-pre ed) ke))

;add-key : Editor Key -> Editor
;The function appends the given Key to the pre part of given Editor,
;IF the length of the Editor has not overflown; otherwise preserves the
; previous state of the Editor


;STRATEGY : Data Decomposition on ed : Editor
(define (add-key ed ke)
  (if (if-no-overflow (make-editor (pre-ke-appender ed ke) (editor-post ed)))
     (make-editor (pre-ke-appender ed ke) (editor-post ed))
      ed))


;backspace : Editor -> Pre-String
;The function removes the last character of the pre part of the String,
;IF the pre field of the Editor is not empty; otherwise returns the empty 
; pre field
(begin-for-test
  (check-equal? (backspace (make-editor "hi" "there"))
                "h"
                "Removed 'i' from 'hi'")
  (check-equal? (backspace (make-editor "" "there"))
                ""
                "Did not remove a character as pre was empty"))
;STRATEGY : Data Decomposition on ed : Editor
(define (backspace ed)
  (if(check-if-pre-empty ed)
  (editor-pre ed)
  (substring (editor-pre ed) START-INDEX (- (editor-pos ed) 1))))

;move-back : Editor -> Editor
;The function returns an Editor after removing the last character of the pre 
; string
(begin-for-test
  (check-equal? (move-back (make-editor "hi" "there"))
                (make-editor (substring "hi" 0 1) "there")
                "Removed 'i' from 'hi' annd appended it with 'there'"))
;STRATEGY : Data Decomposition on ed : Editor
(define (move-back ed)
  (make-editor (backspace ed) (editor-post ed)))

;pre-string-last : Editor -> Pre-String
;The function returns a String, which is the last character of the pre-field of
; the given Editor,
;IF the pre-field is not empty; otherwise returns the empty pre field.
(begin-for-test
  (check-equal? (pre-string-last (make-editor "hi" "there"))
                "i"
                "Extracted 'i'from 'hi'")
  (check-equal? (pre-string-last (make-editor "" "there"))
                ""
                "Didn't extract a character because pre is empty"))
;STRATEGY : Data Decomposition on ed : Editor
(define (pre-string-last ed)
  (if(check-if-pre-empty ed)
  (editor-pre ed)
  (substring (editor-pre ed) (- (editor-pos ed) 1) (editor-pos ed))))

;Post-String is a String, which represents the post part of the Editor
;post-string-appender : Editor -> Post-String
;The function returns a String, 
;after appending the last character of the pre field to the post field of the
; given Editor
(begin-for-test
  (check-equal? (post-string-appender (make-editor "hi" "there"))
                "ithere"
                "Appended 'i'to 'there'"))

(define (post-string-appender ed)
  (string-append (pre-string-last ed) (editor-post ed)))

;move-left : Editor -> Editor
;The function returns an Editor after adding the last character of the pre 
; field to the post field of the given Editor
(begin-for-test
  (check-equal? (move-left (make-editor "hi" "there"))
                (make-editor (substring "hi" 0 1) 
                             (string-append (substring "hi" 1 2) "there"))
                "Appended 'h' to 'there'"))
;STRATEGY : Function composition
(define (move-left ed)
  (make-editor (backspace ed) (post-string-appender ed)))

;post-string-remove-first : Editor -> Post-String
;The function returns the first character of the post field of the given Editor,
;IF the post field is not empty; otherwise returns the empty post-field.

(begin-for-test
  (check-equal? (post-string-remove-first (make-editor "hi" "there"))
                "t"
                "Extracted 't' from 'there'")
  (check-equal? (post-string-remove-first (make-editor "hi" ""))
                ""
                "Did not extract because post field is empty"))
;STRATEGY : Data Decomposition on ed : Editor
(define (post-string-remove-first ed) 
  (if(check-if-post-empty ed)
  (editor-post ed)
  (substring (editor-post ed) START-INDEX FIRST-INDEX)))

;post-string-rest : Editor -> Post-String
;The function returns the post field of the given Editor after removing the 
;first character,
;IF the post field is not empty; otherwise returns the empty post-field.
(begin-for-test
  (check-equal? (post-string-rest (make-editor "hi" "there"))
                "here"
                "Removed 't' from 'there'")
  (check-equal? (post-string-rest (make-editor "hi" ""))
                ""
                "Did not remove because post field is empty"))
;STRATEGY : Data Decomposition on ed : Editor
(define (post-string-rest ed)
  (if(check-if-post-empty ed)
  (editor-post ed)
  (substring (editor-post ed) FIRST-INDEX (string-length (editor-post ed)))))

;pre-string-appender : Editor -> Pre-String
;The fucntion returns the pre part of the given Editor, after suffixing it with the first character of the post part.
(begin-for-test
  (check-equal? (pre-string-appender (make-editor "hi" "there"))
                "hit"
                "Appends 'hi' with 't'"))
;STRATEGY : Data Decomposition on ed : Editor
(define (pre-string-appender ed)
  (string-append (editor-pre ed) (post-string-remove-first ed)))

;move-right : Editor -> Editor
;The function returns an Editor, after appending the first character of the post field to the pre field of the given Editor
(begin-for-test
  (check-equal? (move-right (make-editor "hi" "there"))
                (make-editor (string-append "hi" (substring "there" 0 1)) (substring "there" 1 5))
                "Appends 'hit' with 'here'"))
;STRATEGY : Function composition
(define (move-right ed)
  (make-editor (pre-string-appender ed) (post-string-rest ed)))

;if-no-overflow : Editor -> Boolean
;Returns true if the text in the given Editor does not overflow; false if overflows.
;STRATEGY : Function composition
;(define (if-no-overflow ed)
; (< (+ (image-width (print-pre ed)) (image-width (print-post ed))) WIDTH))
(define (if-no-overflow ed) true)

; string->editor : String -> Editor
; Returns an Editor containing text str and cursor at position 0.
(begin-for-test
  (check-equal? (string->editor "hi")
                (make-editor "" "hi")
                "Makes an Editor containing 'hi' and cursor at position 0 "))

(define (string->editor str)
  (make-editor "" str))

; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor e.
(begin-for-test
  (check-equal? (editor-pos (make-editor "hi" "there"))
                2
                "Returns that the position of the cursor, which is two"))
;STRATEGY : Data Decomposition on ed : Editor
(define (editor-pos ed)
  (string-length (editor-pre ed)))

;check-if-pre-empty : Editor -> Boolean
;Returns true if the pre fied of the Editor is empty; false otherwise
(begin-for-test
  (check-equal? (check-if-pre-empty (make-editor "hi" "there"))
                false
                "The pre field of Editor is not empty")
  (check-equal? (check-if-pre-empty (make-editor "" "there"))
                true
                "The pre field of Editor is empty"))
;STRATEGY : Function composition
(define (check-if-pre-empty ed)
  (if (= (editor-pos ed) 0) true false)) 

;check-if-post-empty : Editor -> Boolean
;Returns true if the post fied of the Editor is empty; false otherwise
(begin-for-test
  (check-equal? (check-if-post-empty (make-editor "hi" "there"))
                false
                "The post field of Editor is not empty")
  (check-equal? (check-if-post-empty (make-editor "hi" ""))
                true
                "The post field of Editor is empty"))
;STRATEGY : Data Decomposition on ed : Editor
(define (check-if-post-empty ed)
  (if (= (string-length (editor-post ed)) 0) true false)) 


;KeyEvent is a String, which is one of the following :
;- "left"
;- "right"
;- Ignores '\t' '\u007F' and '\r'
;-  Accepts all other single-character strings
;edit : Editor KeyEvent -> Editor
;Returns an Editor based on the key pressed
(begin-for-test
  (check-equal? (edit (make-editor "hi" "there") "\t")
                (make-editor "hi" "there")
                "Tab was pressed - Ignored")
  (check-equal? (edit (make-editor "hi" "there") "\u007F")
                (make-editor "hi" "there")
                "Delete was pressed - Ignored")
  (check-equal? (edit (make-editor "hi" "there") "\r")
                (make-editor "hi" "there")
                "Enter was pressed - Ignored")
  (check-equal? (edit (make-editor "hi" "there") "c")
                (add-key (make-editor "hi" "there") "c")
                "C was pressed. Key added at cursor.")
  (check-equal? (edit (make-editor "hi" "there") "\b")
                (move-back (make-editor "hi" "there"))
                "Backspace was pressed. Character to the left of the cursor removed")
  (check-equal? (edit (make-editor "hi" "there") "left")
                (move-left (make-editor "hi" "there"))
                "Left was pressed. Moves cursor one character to the left")
  (check-equal? (edit (make-editor "hi" "there") "right")
                (move-right (make-editor "hi" "there"))
                "Right was pressed. Moves cursor one character to the right"))
;STRATEGY : Data Decomposition on ke
;TEMPLATE :
;(define (edit ed ke)
;    (cond [ (string=? ke "\t") ...]
;          [ (string=? ke "\u007F") ...]
;          [ (string=? ke "\r") ...]
;          [ (string=? ke "\b") ...]
;          [ (= (string-length ke) 1) ...]
;          [ (string=? ke "left") ...]
;          [ (string=? ke "right") ...]
;          [ else ...]))
(define (edit ed ke) 
   (cond [(string=? ke "\t") ed]
         [(string=? ke "\u007F") ed]
         [(string=? ke "\r") ed]
         [(string=? ke "\b") (move-back ed)]
         [(= (string-length ke) 1) (add-key ed ke)]
         [(string=? ke "left") (move-left ed)]
         [(string=? ke "right") (move-right ed)]
         [else ed]))
        

;run : String -> World
;Starts the Simulation
(define (run s)   
   (big-bang (string->editor s)
            [to-draw render]
            [on-key edit]))



         

         