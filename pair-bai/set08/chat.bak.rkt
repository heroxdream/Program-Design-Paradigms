;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chat) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 0)


(provide mk-world)
(provide receive)
(provide key-handler)
(provide get-users)
(provide get-editor)
(provide get-editor-pre)
(provide get-editor-post)
(provide get-chat-history)





; A UserName is a String, consisting of only letters and numbers,
; and is between 1 and 12 characters long.
; Represents a chat room participant.
 
; A Message is a String
 
; MsgToServer is a:
; - (list 'broadcast Message)
; - (list 'private UserName Message)
; The user in the private message is the intended recipient of the message.

; Template:
(define (mts-to-server-fn mts)
  (cond
    [(check-type? 'broadcast) (...(first mts)...(second mts)...)]
    [(check-type? 'private) (...(first mts)...(second mts)...(third mts)...)]))


; A MsgFromServer is a:
; - (list 'userlist ListOf<UserName>) ; all the current chat participants
; - (list 'join UserName) ; the user that just joined
; - (list 'leave UserName) ; the user that just left
; - (list 'error Message) ; an error message from the server
; - (list 'private UserName Message) ; a priv msg from the specified user
; - (list 'broadcast UserName Message) ; a broadcast msg from the specified user

; Template:
(define (msg-from-server-fn mfs)
  (cond
    [(check-type? 'userlist (first mfs)) 
     (...(first mfs)... (list-fn (second mfs)))]
    [(check-type? 'join (first mfs)) 
     (...(first mfs)... (second mfs))]
    [(check-type? 'leave (first mfs))
     (...(first mfs)... (second mfs))]
    [(check-type? 'error (first mfs))
     (...(first mfs)... (second mfs))]
    [(check-type? 'private (first mfs))
     (...(first mfs)... (second mfs)...(third mfs)...)]
    [(check-type? 'broadcast (first mfs))
     (...(first mfs)... (second mfs)...(third mfs)...)]))

; Symbol Symbol -> Boolean
(define (check-type? type symbol)
  (eq? symbol type))

(define-struct event [msg type])
; A Event is a (make-event Message Symbol)
; INTERP: 

(define-struct world [editor users events])
; A World is a (make-world Editor ListOf<UserName> ListOf<Event>)
; INTERP: Represents the state of the world containing the editor, users and
; events 

; Template:
(define (world-fn w)
  (...(editor-fn (world-editor w))...
   ...(list-fn (world-users w))...
   ...(list-fn (world-events w))...))

(define CP-HEIGHT 400)

(define CP-WIDTH 100)

(define EDITOR-WIDTH 300)

(define EDITOR-HEIGHT 20)

(define MSG-HEIGHT 380)

(define MSG-WIDTH 300)

(define SCENE-WIDTH 400)

(define SCENE-HEIGHT 400)

(define CP (empty-scene CP-WIDTH CP-HEIGHT))

(define EDITOR (empty-scene EDITOR-WIDTH EDITOR-HEIGHT))

(define MSG (empty-scene MSG-WIDTH MSG-HEIGHT))

(define MT (empty-scene SCENE-WIDTH SCENE-HEIGHT))

(define COLON ":")

(define CLIENT-NAME "xuan")

(define FONT-SIZE 12)

(define FONT-COLOR "black")

(define MAX-EVENTS 25)

(define w 10)

(define (render w)
  (place-images
   (list (render-chat-participants (world-users w))
         (render-message-area (world-events w))
         (render-editor (world-editor w)))
   (list (make-posn 50 200)
         (make-posn 250 190)
         (make-posn 250 390))
   MT))

(define (render-chat-participants users)
  (overlay/align "left" "top" 
               (apply above/align (truncated-lst users 'userlist))
               CP))


; List -> List
(define (truncated-lst lst symbol)
  (local (
          (define param-lst (append (list "left" 
                                          (line 0 0 "white"))
                                    (strings->images lst symbol))))
    (truncate param-lst CP-HEIGHT)))
    
; ListOf<UserName> -> ListOf<UserName>
(define (remove-last lst)
  (reverse (rest (reverse lst))))


; ListOf<String> -> ListOf<Image>
(define (strings->images lst symbol)
  (map (lambda (str) (text str FONT-SIZE (text-color symbol)))
       lst))



(define (render-message-area events)
  (overlay/align "left" "top" 
               (apply above/align (truncated-event-image (events-mul events)))
               MSG))

(define (truncated-event-image events-imgs)
  (local (
          (define param-lst (append (list "left" 
                                          (line 0 0 "white")
                                          (line 0 0 "white"))
                                    events-imgs)))
    (append (list "left" 
                  (line 0 0 "white")
                  (line 0 0 "white"))
            (reverse (rest (rest (rest (truncate param-lst MSG-HEIGHT))))))))





(define (truncate lst height)
  (if (<= (image-height (apply above/align lst)) (add1 height))
      lst
      (truncate (remove-last lst) height)))











; ListOf<Event> -> ListOf<Image>
(define (events-mul events0)
  (local (
          (define (events-mul/a events a)
            (cond 
              [(empty? events) a]
              [else (events-mul/a (rest events) 
                                  (append a (event->image (first events))))]))
          )
    (events-mul/a events0 '())))

; Event -> ListOf<Image>
(define (event->image e)
  (local (
          (define ori-image (text (event-msg e) 12 (text-color (event-type e))))
          
          (define (split e)
            (local (
                    (define pos (find-pos (event-msg e) MSG-WIDTH))
                    (define segment1 (substring (event-msg e) 0 pos))
                    (define segment2 (substring (event-msg e) pos))
                    )
              (append (event->image (make-event segment2 (event-type e)))
                      (event->image (make-event segment1 (event-type e))))))
          
          
          )
    (if (<= (image-width ori-image) MSG-WIDTH)
        (list ori-image)
        (split e))))

; String Natural -> Natural
(define (find-pos str width)
  (local (
          (define (find-pos/a str width a)
            (if (<= (image-width (text (substring str 0 a) 12 "black")) width)
                a
                (find-pos/a str width (sub1 a))))
          )
    (find-pos/a str width (string-length str))))



(define (text-color symbol)
  (if (or (eq? symbol 'broadcast) (eq? symbol 'userlist))
      "black"
      (if (eq? symbol 'private)
          "blue"
          (if (eq? symbol 'error)
              "red"
              "gray"))))




; run : UserName IPAddress -> World
; Connect to the given chat server with user name nam.
(define (run nam server)
  (big-bang (mk-world nam)
            (on-receive receive)
            (to-draw render)
            (on-key key-handler)
            (name nam)
            (register server)
            (port 5010)))








; A HandlerResult is a:
; - World
; - (make-package World MsgToServer)
 
; mk-world : UserName -> World
; Returns the initial world state for user name.
(define (mk-world nam)
  (make-world (make-editor "" "")
              (list nam)
              '()))
 
; receive : World MsgFromServer -> HandlerResult
; Handles messages received from the server.
(define (receive w mfs)
  (cond
    [(check-type? 'userlist (first mfs)) (update-userlist w (second mfs))]
    [(check-type? 'join (first mfs)) (add-username w (second mfs))]
    [(check-type? 'leave (first mfs)) (delete-username w (second mfs))]
    [(check-type? 'error (first mfs)) (display-error w (second mfs))]
    [(check-type? 'private (first mfs)) 
     (receive-private-msg w (second mfs) (third mfs))]
    [(check-type? 'broadcast (first mfs))
     (receive-broadcast-msg w (second mfs) (third mfs))]))

;CAN BE ABSTRACTED WITH ADD
; World String -> World
(define (delete-username w username)
 (make-world (world-editor w)
             (sort (remove username
                           (world-users w))
                   string<?)
              (update-events (world-events w) 'leave 
                             (string-append username " has left the chat."))))


; World String -> World
(define (add-username w username)
  (make-world (world-editor w)
              (sort (append (world-users w)
                            (list username))
                    string<?)
              (update-events (world-events w) 'join 
                             (string-append username " joined"))))


; ListOf<Event> Symbol Message -> ListOf<Event>
(define (update-events events symbol msg)
  (if (= (length events) MAX-EVENTS)
      (append (list (make-event msg symbol)) (remove-last events))
      (append (list (make-event msg symbol)) events)))



; World ListOf<UserName> -> World
(define (update-userlist w list-usernames)
  (make-world (world-editor w)
              (sort list-usernames string<? )
              (world-events w)))

; World Message -> World
(define (display-error w msg)
  (make-world (world-editor w)
              (world-users w)
              (update-events (world-events w) 'error msg)))

;CAN BE ABSTRACTED WITH BROADCAST
; World Username Message -> World
(define (receive-private-msg w username msg)
  (make-world (world-editor w)
              (world-users w)
              (update-events (world-events w) 'private 
                             (string-append "< " username " > " msg))))

;World Username Message -> World 
(define (receive-broadcast-msg w username msg)
  (make-world (world-editor w)
              (world-users w)
              (update-events (world-events w) 'broadcast 
                             (string-append "< " username " > " msg))))

; key-handler : World KeyEvent -> HandlerResult
; Handles keyboard user input.
(define (key-handler w ke)
   (cond 
     [(string=? ke "\r") (send-message w)]
     [(string=? ke "\t") (prompt w)]
     [else 
      (make-world (edit (world-editor w) ke)
                  (world-users w)
                  (world-events w))]))

; World -> HandlerResult
(define (send-message w)
  (local ((define msg-type (return-msg-type w)))
    (make-package (make-world (make-editor "" "")
                              (world-users w)
                              (update-events (world-events w) msg-type
                                             (msg-assemble (world-editor w) 
                                                           msg-type)))
                  (make-msg msg-type (world-editor w)))))


; Symbol Editor -> MsgToServer
(define (make-msg msg-type ed)
  (if (check-type? 'broadcast msg-type)
      (list 'broadcast (editor->string ed))
      (local ((define target-name (extract-string (editor->string ed) COLON))
              (define pos-after-colon (string-length target-name)))
        (list 'private target-name 
              (substring  (editor->string ed) pos-after-colon)))))


; World -> Symbol
(define (return-msg-type w)
  (local ((define user-lst (world-users w))
          (define editor-text (editor->string (world-editor w)))
          (define contains-colon? (string-contains? COLON editor-text)))
    (if (and contains-colon? 
             (member? (extract-string editor-text COLON) user-lst))
        'private
        'broadcast)))

; String 1String -> String
(define (extract-string str seprator)
  (local ((define (find-pos/a str seprator a)
            (if (string=? (string-ith str a) seprator)
                a
                (find-pos/a str seprator (add1 a)))))
    (substring str 0 (find-pos/a str seprator 0))))




; Editor Symbol -> Message
(define (msg-assemble ed msg-type)
  (if (check-type? 'broadcast msg-type)
      (string-append "< " CLIENT-NAME " > " (editor->string ed))
      (local ((define target-name (extract-string (editor->string ed) COLON))
              (define pos-after-colon (string-length target-name)))
        (string-append "< " CLIENT-NAME "->" 
                       target-name " > "
                       (substring  (editor->string ed) pos-after-colon)))))





; World -> World
(define (prompt w)
  (make-world (prompt-username (world-editor w) 
                               (world-users w))
              (world-users w)
              (world-events w)))

; Editor Userlist -> Editor 
(define (prompt-username ed userlist)
  (if (and (not (check-if-pre-empty ed))
           (check-if-post-empty ed))
      (update-editor ed userlist)
      ed))


; Editor Userlist -> Editor
(define (update-editor ed userlist)
  (local ((define pre (editor-pre ed))
          ;Editor ListOf<UserName> -> ListOf<UserName>
          (define (get-username-matches ed userlist)
            (filter (lambda (x) (prefix? pre x)) userlist))
          (define matched-user-names (get-username-matches ed userlist))
          )
    (if (empty? matched-user-names)
        ed
        (make-editor (first matched-user-names) ""))))
    
          
; String String -> Boolean
(define (prefix? pre str)
  (and (>= (string-length str) (string-length pre))
       (string=? pre
                 (substring str 0 (string-length pre) ))))

 
; get-users : World -> ListOf<UserName>
; Returns a list of current chat participants, in lexicographic order.
(define (get-users w)
  (world-users w)) 


; get-editor : World -> Editor
; Returns a representation of the chat client's input area.
(define (get-editor w)
  (world-editor w))
 
; get-editor-pre : Editor -> String
; get-editor-post : Editor -> String
; Returns an editor's content before and after the cursor.
(define (get-editor-pre e)
  (editor-pre e))
(define (get-editor-post e)
  (editor-post e))

; get-chat-history : World -> ListOf<String>
; Returns a list of chat events, rendered to string, 
; where each string format is the same as when the event is
; rendered to the chat window.(define MT (empty-scene 400 400))
(define (get-chat-history w)
  (world-events w))









;global constants -----------------------------------
;RECTANGLE is a rectangle, which represents the cursor in the editor
(define RECTANGLE (rectangle 1 20 "solid" "red"))
;START-INDEX represents the first position of a string
(define START-INDEX 0)
;FIRST-INDEX represents the second position of a string
(define FIRST-INDEX 1)
;TEXT-SIZE represents the size of the text displayed in the text editor
(define TEXT-SIZE 12)
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
                (text "hi" 12 "black")
                "Prints Text containing 'hi' "))
;STRATEGY : Data Decomposition on ed : Editor
(define (print-pre ed)
  (text (editor-pre ed) TEXT-SIZE TEXT-COLOR))

;print-post : Editor-> Text 
;Returns Text that contains the post part of the Editor
(begin-for-test
  (check-equal? (print-post (make-editor "hi" "there"))
                (text "there" 12 "black")
                "Prints Text containing 'there' "))
;STRATEGY : Data Decomposition on ed : Editor
(define (print-post ed)
  (text (editor-post ed) TEXT-SIZE TEXT-COLOR))

;render : Editor -> Image
;Returns an Image that contains a cursor between the pre and post parts 
; of the given Editor,
;on a empty-scene background

;STRATEGY : Function Composition
(define (render-editor ed) 
  (overlay/align "left" "center"
                 (if (no-overflow? ed)
                     (beside (print-pre ed) RECTANGLE (print-post ed))
                     (beside (text (str-within-view ed) TEXT-SIZE TEXT-COLOR)
                             RECTANGLE))
                 EDITOR))

; Editor -> String
(define (str-within-view ed)
  (local ((define str-pre (editor-pre ed)))
    (substring str-pre (find-pos-in-pre str-pre 300))))

; String Natural -> Natural
(define (find-pos-in-pre pre width)
  (local (
          (define (find-pos-in-pre/a pre width a)
            (if 
                (or (= a 0)
                    (> (image-width (string->img (substring pre (sub1 a))))
                       width))
                a
                (find-pos-in-pre/a pre width (sub1 a)))))
    (find-pos-in-pre/a pre width (string-length pre))))

; String -> Image
(define (string->img s)
  (text s TEXT-SIZE "black"))


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
  (make-editor (pre-ke-appender ed ke) (editor-post ed)))


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
  (make-editor (backspace ed) 
                          (editor-post ed)))

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
  (make-editor (backspace ed) 
               (post-string-appender ed)))

;post-string-remove-first : Editor -> Post-String
;The function returns the first character of the post field of the given 
; Editor, IF the post field is not empty
; otherwise returns the empty post-field.

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
;The fucntion returns the pre part of the given Editor, after suffixing it with 
;the first character of the post part.
(begin-for-test
  (check-equal? (pre-string-appender (make-editor "hi" "there"))
                "hit"
                "Appends 'hi' with 't'"))
;STRATEGY : Data Decomposition on ed : Editor
(define (pre-string-appender ed)
  (string-append (editor-pre ed) (post-string-remove-first ed)))

;move-right : Editor -> Editor
;The function returns an Editor, after appending the first character of the 
; post field to the pre field of the given Editor
(begin-for-test
  (check-equal? (move-right (make-editor "hi" "there"))
                (make-editor (string-append "hi" 
                                            (substring "there" 0 1)) 
                             (substring "there" 1 5))
                "Appends 'hit' with 'here'"))
;STRATEGY : Function composition
(define (move-right ed)
  (make-editor (pre-string-appender ed) (post-string-rest ed)))

;no-overflow? : Editor -> Boolean
;Returns true if the text in the given Editor does not overflow; false 
; if overflows.
;STRATEGY : Function composition
(define (no-overflow? ed)
 (< (image-width (print-pre ed)) EDITOR-WIDTH))

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
  (check-equal? (edit (make-editor "hi" "there") "\u007F")
                (make-editor "hi" "there")
                "Delete was pressed - Ignored")
  (check-equal? (edit (make-editor "hi" "there") "c")
                (add-key (make-editor "hi" "there") "c")
                "C was pressed. Key added at cursor.")
  (check-equal? (edit (make-editor "hi" "there") "\b")
                (move-back (make-editor "hi" "there"))
                "Backspace was pressed. Character to the left of 
                 the cursor removed")
  (check-equal? (edit (make-editor "hi" "there") "left")
                (move-left (make-editor "hi" "there"))
                "Left was pressed. Moves cursor one character to the left")
  (check-equal? (edit (make-editor "hi" "there") "right")
                (move-right (make-editor "hi" "there"))
                "Right was pressed. Moves cursor one character to the right"))
;STRATEGY : Data Decomposition on ke
(define (edit ed ke) 
   (cond [(string=? ke "\u007F") ed]
         [(string=? ke "\b") (move-back ed)]
         [(= (string-length ke) 1) (add-key ed ke)]
         [(string=? ke "left") (move-left ed)]
         [(string=? ke "right") (move-right ed)]
         [else ed]))


; Editor -> String
(define (editor->string e)
  (string-append (editor-pre e) (editor-post e)))