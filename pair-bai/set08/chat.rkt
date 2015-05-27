;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chat) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 30)

(provide mk-world)
(provide receive)
(provide key-handler)
(provide get-users)
(provide get-editor)
(provide get-editor-pre)
(provide get-editor-post)
(provide get-chat-history)

;global constants

(define CP-HEIGHT 400) ;userlist height

(define CP-WIDTH 100) ;userlist width

(define EDITOR-WIDTH 300) ;editor width

(define EDITOR-HEIGHT 20) ;editor height

(define MSG-HEIGHT 380) ;event area height

(define MSG-WIDTH 300) ;event area width

(define SCENE-WIDTH 400) ;empty-scene width

(define SCENE-HEIGHT 400) ;empty-scene height
 
(define CP (empty-scene CP-WIDTH CP-HEIGHT)) ;userlist area

(define EDITOR (empty-scene EDITOR-WIDTH EDITOR-HEIGHT)) ;editor area

(define MSG (empty-scene MSG-WIDTH MSG-HEIGHT)) ;events area

(define MT (empty-scene SCENE-WIDTH SCENE-HEIGHT)) ;empty-scene

(define COLON ":")

(define FONT-SIZE 12)

(define FONT-COLOR "black")
;maximum number of events that can exist
(define MAX-EVENTS 25)

;RECTANGLE is a rectangle, which represents the cursor in the editor
(define RECTANGLE (rectangle 1 20 "solid" "red"))
;START-INDEX represents the first position of a string
(define START-INDEX 0)
;FIRST-INDEX represents the second position of a string
(define FIRST-INDEX 1)

;Editor Structure Definition --------------------

;Editor is a structure of the type (make-editor String String),
;INTERP :which means the text in the editor is (string-append s t) with the
; cursor displayed between s and t
(define-struct editor [pre post])
(define editor1 (make-editor "1" "2"))
; TEMPLATE:
; editor-fn : Editor -> ???
; STRAGEGY: data decomposition on e : Editor
;(define (editor-fn e)
;  (... (editor-pre e) ... (editor-post e) ...))

; A UserName is a String, consisting of only letters and numbers,
; and is between 1 and 12 characters long.
; Represents a chat room participant.
(define user1 "jack")
 
; A Message is a String
; INTERP: which represents the message sent by the user  
(define msg1 "hi") 

; MsgToServer is a:
; - (list 'broadcast Message)
; - (list 'private UserName Message)
; The user in the private message is the intended recipient of the message.

; Template:
; mts-to-server-fn : MsgToServer -> ???
; STRAGEGY: data decomposition on mts : MsgToServer
(define (mts-to-server-fn mts)
  (cond
    [(check-type? 'broadcast (first mts)) 
     (...(first mts)...(second mts)...)]
    [(check-type? 'private (first mts)) 
     (...(first mts)...(second mts)...(third mts)...)]))

; A MsgFromServer is a:
; - (list 'userlist ListOf<UserName>) ; all the current chat participants
; - (list 'join UserName) ; the user that just joined
; - (list 'leave UserName) ; the user that just left
; - (list 'error Message) ; an error message from the server
; - (list 'private UserName Message) ; a priv msg from the specified user
; - (list 'broadcast UserName Message) ; a broadcast msg from the specified user
; INTERP : which represents the message coming from the server
; Template:
; msg-from-server-fn : MsgFromServer -> ???
; STRAGEGY: data decomposition on mfs : MsgFromServer
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

; check-type? : Symbol Symbol -> Boolean
; Checks type of the message from sever
; STRATEGY : Function strategy 
(begin-for-test
  (check-true (check-type? 'broadcast 'broadcast)
              "Type is broadcast"))
(define (check-type? type symbol)
  (eq? symbol type))

; A HandlerResult is a:
; - World
; - (make-package World MsgToServer)

;TEMPLATE
;handler-result-fn : HandlerResult -> ???
;STRATEGY: Data decomposition on h : HandlerResult
(define (handler-result-fn h)
  (cond 
    [(world? h) (world-fn h)]
    [else
     (...(world-fn (first h)) ... (mts-fn (second h)))]))

(define-struct event [msg type])
; A Event is a (make-event Message Symbol)
; INTERP: which represents a event between the server and client during the 
;         chatting. Msg represents the content of the event and type represents
;         the message type.
(define e1 (make-event "hi" 'broadcast))
; Template: 
; event-fn : Event -> ???
; STRATEGY : Data decomposition on e : Event
(define (event-fn e)
  (... (event-msg e) ... (event-type e) ...))

(define-struct world [editor users events client])
; A World is a (make-world Editor ListOf<UserName> ListOf<Event> UserName)
; INTERP: Represents the state of the world containing the editor, users and
; events 
(define world1 (make-world editor1 user1 e1 "jack"))
; Template:
; world-fn : World -> ???
; STRATEGY : Data decomposition on w : World
(define (world-fn w)
  (...(editor-fn (world-editor w))...
   ...(list-fn (world-users w))...
   ...(list-fn (world-events w))...
   ... (world-client w)...))

;World functions begin

; render : World -> Image
; Renders the world
; STRATEGY: Data decomposition on w: World
(begin-for-test 
  (check-equal? (render (mk-world "Rob"))
               (place-images
                (list (render-chat-participants (world-users (mk-world "Rob")))
                      (render-message-area (world-events (mk-world "Rob")))
                      (render-editor (world-editor (mk-world "Rob"))))
                (list (make-posn 50 200)
                      (make-posn 250 190)
                      (make-posn 250 390))
                MT)
               "Renders the world"))
(define (render w)
  (place-images
   (list (render-chat-participants (world-users w))
         (render-message-area (world-events w))
         (render-editor (world-editor w)))
   (list (make-posn 50 200)
         (make-posn 250 190)
         (make-posn 250 390))
   MT))

; render-chat-participants : ListOf<UserName> -> Image
; Renders the list of users
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (render-chat-participants (list "Rob" "Jacob" "Damien"))
                (overlay/align "left" "top" 
                (apply above/align (truncated-lst (list "Rob" "Jacob" "Damien")
                                                 'userlist))
                CP)
                "renders chat participants"))
(define (render-chat-participants users)
  (overlay/align "left" "top" 
               (apply above/align (truncated-lst users 'userlist))
               CP))

; render-message-area ; ListOf<Event> -> Image
; Renders the event section
; STRATEGY : Function composition 
(begin-for-test
  (check-equal? (render-message-area (list (make-event "hi" 'broadcast)
                                           (make-event "this is a msg"
                                                       'broadcast)))
                (overlay/align "left" "top" 
                (apply above/align 
                      (truncated-event-image 
                       (events-mul (list (make-event "hi" 'broadcast)
                                           (make-event "this is a msg"
                                                       'broadcast)))))
                MSG)
                "Renders events"))
(define (render-message-area events)
  (overlay/align "left" "top" 
               (apply above/align (truncated-event-image (events-mul events)))
               MSG))

; truncated-lst : ListOf<UserName> -> ListOf<UserName>
; Returns a truncated list of usernames
; STRATEGY : Function compostion
(begin-for-test 
  check-equal? (truncated-lst (list "ab" "cd" "de") 'broadcast)
              (list "left" (line 0 0 "white")
                    (text "ab" 12 "black")
                    (text "cd" 12 "black")
                    (text "ef" 12 "black")) 
              "Truncated the list")
(define (truncated-lst lst symbol)
  (local ((define param-lst (append (list "left" 
                                          (line 0 0 "white"))
                                    (strings->images lst symbol))))
    (truncate param-lst CP-HEIGHT)))
    
; remove-last : ListOf<UserName> -> ListOf<UserName>
; Deletes the last element from the list
; STRATEGY : data decomposition on lst : ListOf<UserName>
(begin-for-test 
  check-equal? (remove-last (list "ab" "cd" "de"))
               (list "ab" "cd") 
               "Removed de")
(define (remove-last lst)
  (reverse (rest (reverse lst))))

; strings->images : ListOf<String> -> ListOf<Image>
; Converts the list of strings into a list of images
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (strings->images (list "ab" "cd" "ef") 'userlist)
                (list (text "ab" 12 "black")
                      (text "cd" 12 "black")
                      (text "ef" 12 "black"))
                "Converted to images"))
(define (strings->images lst symbol)
  (map (lambda (str) (text str FONT-SIZE (text-color symbol)))
       lst))

; truncated-event-image : ListOf<Image> -> ListOf<Any>
; Returns the truncated list of events as images
; STRATEGY : data decomposition on lst : ListOf<Image>
(begin-for-test
  (check-equal? (truncated-event-image (list (text "ab" 12 "black")
                                             (text "cd" 12 "black")
                                             (text "ef" 12 "black")))
                (list "left"
                      (line 0 0 "white") 
                      (line 0 0 "white") 
                      (text "ef" 12 "black")
                      (text "cd" 12 "black")
                      (text "ab" 12 "black"))
                "Truncated"))
(define (truncated-event-image events-imgs)
  (local ((define param-lst (append (list "left" 
                                          (line 0 0 "white")
                                          (line 0 0 "white"))
                                    events-imgs)))
    (append (list "left" 
                  (line 0 0 "white")
                  (line 0 0 "white"))
            (reverse (rest (rest (rest (truncate param-lst MSG-HEIGHT))))))))

; truncate : ListOf<Any> Natural -> ListOf<Any> 
; Truncates the list of events if it goes beyond limit
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (truncate (list "left" (text "ab" 12 "black")
                      (text "cd" 12 "black")
                      (text "ef" 12 "black")) 100)
                (list "left" (text "ab" 12 "black")
                             (text "cd" 12 "black") 
                             (text "ef" 12 "black"))
                "Truncated list")
  (check-equal? (truncate (list "left" (line 0 0 "white")
                                (text "ab" 12 "black")
                                (text "cd" 12 "black")
                                (text "ef" 12 "black")) 20)
                (list "left" (line 0 0 "white") (text "ab" 12 "black"))
                "Truncated list"))
(define (truncate lst height)
  (if (<= (image-height (apply above/align lst)) (add1 height))
      lst
      (truncate (remove-last lst) height)))

; event-mul : ListOf<Event> -> ListOf<Image>
; Converts a list of events into a list of images
; STRATEGY : Data decomposition on events : ListOf<Event>
(begin-for-test
  (check-equal? (events-mul (list
                (make-event "Batman joined" 'join)))
                (list (text "Batman joined" 12 "gray"))
                "Converted into image"))
(define (events-mul events0)
  (local (;ListOf<Event> ListOf<Image> -> ListOf<Image>
          ;Converts events onto conv-so-far
          ;WHERE: conv-so-far is the elements of events0 converted so far, and
          ;       events is the elements of events0 to be converted.
          ;STRATEGY: data decomposition on events: ListOf<Event>
          (define (events-mul/a events conv-so-far)
            (cond 
              [(empty? events) conv-so-far]
              [else (events-mul/a (rest events) 
                                  (append conv-so-far 
                                          (event->image (first events))))])))
    (events-mul/a events0 '())))

; event->image : Event -> ListOf<Image>
; Converts an event into a list of images
; STRATEGY : Data decomposition on e : Event
(begin-for-test
  (check-equal? (event->image (make-event "Batman joined" 'join))
                (list (text "Batman joined" 12 "gray"))
                "Converted into image")
  (check-true   (list? (event->image 
                      (make-event "This is to make sure the width is exceede
                d by the specified width for testing purposes" 'broadcast)))))
(define (event->image e)
  (local ((define ori-image (text (event-msg e) 12 (text-color (event-type e))))
          ;split : Event -> ListOf<Image>
          ;Splits an event into two parts if it does not fit into the given 
          ;width
          ;STRATEGY: Data decomposition on e : Event
          (define (split e)
            (local ((define pos (find-pos (event-msg e) MSG-WIDTH))
                    (define segment1 (substring (event-msg e) 0 pos))
                    (define segment2 (substring (event-msg e) pos)))
              (append (event->image (make-event segment2 (event-type e)))
                      (event->image (make-event segment1 (event-type e)))))))
    (if (<= (image-width ori-image) MSG-WIDTH)
        (list ori-image)
        (split e))))

; find-pos : String Natural -> Natural
; return the position of the string, at which point, if the string is converted
; to a image, it will exceed the predefined width.
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (find-pos "hello" 10)
                1
                "Found the position"))
(define (find-pos str0 width)
  (local (;String Natural Natural -> Natural
          ;return the position of the string, at which point, if the string is 
          ;converted to a image, it will exceed the predefined width.
          ;WHERE: pos-from-end is the position from the end of str
          ;STRATEGY : Function composition  
          (define (find-pos/a str width pos-from-end)
            (if (<= (image-width 
                     (text (substring str 0 pos-from-end) 
                           FONT-SIZE FONT-COLOR)) width)
                pos-from-end
                (find-pos/a str width (sub1 pos-from-end)))))
    (find-pos/a str0 width (string-length str0))))
 
; text-color : Symbol -> String
; Returns the colour to render the text depending on type of message
; STRATEGY : Function composition
(begin-for-test 
  (check-equal? (text-color 'private)
                "blue"
                "Private message")
  (check-equal? (text-color 'error)
                "red"
                "Error message")
  (check-equal? (text-color 'join)
                "gray"
                "Join message"))
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

; mk-world : UserName -> World
; Returns the initial world state for given username.
(begin-for-test
  (check-equal? 
   (mk-world "Rob")
             (make-world (make-editor "" "")
                         (list "Rob")
                         empty
                         "Rob")
   "Returned world state"))
(define (mk-world nam)
  (make-world (make-editor "" "")
              (list nam)
              '()
              nam))

; receive : World MsgFromServer -> HandlerResult
; Handles messages received from the server.
; STRATEGY : Data decomposiiton on mfs : MsgFromServer
(begin-for-test
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'userlist (list "Harvey" "Ian" "Viola" "Sam" "Bridge")))
   (make-world (make-editor "" "")
               (second 
                (list 'userlist (list "Bridge" "Harvey" "Ian" "Sam" "Viola"))) 
               (list (make-event "set07 joined." 'join))
               "John")
   "Update userlist on userlist type message")
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'join "Batman"))
   (make-world (make-editor "" "")
               (list "Batman" "Dan" "Jim")
               (list
                (make-event "Batman joined" 'join)
                (make-event "set07 joined." 'join))
               "John")
   "New User joined")
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'join "Batman"))
   (make-world (make-editor "" "")
               (list "Batman" "Dan" "Jim")
               (list
                (make-event "Batman joined" 'join)
                (make-event "set07 joined." 'join))
               "John")
   "New User joined")
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'leave "Dan"))
   (make-world (make-editor "" "")
               (list "Jim")
               (list
                (make-event
                 "Dan has left the chat."
                 'leave)
                (make-event "set07 joined." 'join))
               "John")
   "Dan left")
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'error "Dan"))
   (make-world (make-editor "" "")
               (list "Jim" "Dan")
               (list
                (make-event "Dan" 'error)
                (make-event "set07 joined." 'join))
               "John")
   "Error message")
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'private "Dan" "hi"))
   (make-world (make-editor "" "")
               (list "Jim" "Dan")
               (list
                (make-event "< Dan > hi" 'private)
                (make-event "set07 joined." 'join))
               "John")
   "private message")
  (check-equal? 
   (receive 
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) 
    (list 'broadcast "Dan" "hello"))
   (make-world (make-editor "" "")
               (list "Jim" "Dan")
               (list
                (make-event "< Dan > hello" 'broadcast)
                (make-event "set07 joined." 'join))
               "John")
   "broadcast message"))
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

; delete-username : World String -> World
; Deletes a username from the list of usernames
; STRATEGY : Data decomposition on w : World
(begin-for-test 
  (check-equal? 
   (delete-username
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) "Dan")
   (make-world (make-editor "" "")
               (list "Jim")
               (list
                (make-event
                 "Dan has left the chat."
                 'leave)
                (make-event "set07 joined." 'join))
               "John")
   "Deleted Dan"))
(define (delete-username w username)
 (make-world (world-editor w)
             (sort (remove username
                           (world-users w))
                   string-case-insensitive<?)
              (update-events (world-events w) 'leave 
                             (string-append username " has left the chat."))
              (world-client w)))

; add-username : World String -> World
; Adds a username to the list of usernames
; STRATEGY : Data decomposition on w : World
(begin-for-test 
  (check-equal? 
   (add-username
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) "Blake")
   (make-world (make-editor "" "")
               (list "Blake" "Dan" "Jim")
               (list
                (make-event "Blake joined" 'join)
                (make-event "set07 joined." 'join))
               "John")
   "Added Blake"))
(define (add-username w username)
  (make-world (world-editor w)
              (sort (append (world-users w)
                            (list username))
                    string-case-insensitive<?)
              (update-events (world-events w) 'join 
                             (string-append username " joined"))
              (world-client w)))

; to-lower : String -> String
; Lowers all the characters of a string
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (to-lower "TEST MSG")
                "test msg"
                "Converted all to lower characters"))
(define (to-lower str)
  (list->string (map (lambda (char) (char-downcase char))
       (string->list str))))

; string-case-insensitive<? : String String -> Boolean
; Returns true if the given two strings are in ascending order
; STRATEGY : Function composition
(begin-for-test
  (check-true (string-case-insensitive<? "hi" "there")
               "In ascending order")
  (check-false (string-case-insensitive<? "xat" "there")
               "Not in ascending order"))
(define (string-case-insensitive<? str1 str2)
  (string<? (to-lower str1) (to-lower str2)))

; update-events : ListOf<Event> Symbol Message -> ListOf<Event>
; Updates the events 
; STRATEGY : Function composiiton
(begin-for-test
  (check-equal? (update-events 
    (list (make-event "hi" 'broadcast)
          (make-event "this" 'broadcast)
          (make-event "is" 'broadcast)
          (make-event "a" 'broadcast)
          (make-event "message" 'broadcast)
          (make-event "to" 'broadcast)
          (make-event "test" 'broadcast)
          (make-event "whether" 'broadcast)
          (make-event "this " 'broadcast)
          (make-event "function" 'broadcast)
          (make-event "will" 'broadcast)
          (make-event "accept" 'broadcast)
          (make-event "events" 'broadcast)
          (make-event "that" 'broadcast)
          (make-event "are" 'broadcast)
          (make-event "more" 'broadcast)
          (make-event "than" 'broadcast)
          (make-event "twenty" 'broadcast)
          (make-event "five" 'broadcast)
          (make-event "in." 'broadcast)
          (make-event "number" 'broadcast)
          (make-event "testing" 'broadcast)
          (make-event "if" 'broadcast)
          (make-event "is" 'broadcast)
          (make-event "this." 'broadcast)) 'broadcast "hi")
                (list
                 (make-event "hi" 'broadcast)
                 (make-event "hi" 'broadcast)
                 (make-event "this" 'broadcast)
                 (make-event "is" 'broadcast)
                 (make-event "a" 'broadcast)
                 (make-event "message" 'broadcast)
                 (make-event "to" 'broadcast)
                 (make-event "test" 'broadcast)
                 (make-event "whether" 'broadcast)
                 (make-event "this " 'broadcast)
                 (make-event "function" 'broadcast)
                 (make-event "will" 'broadcast)
                 (make-event "accept" 'broadcast)
                 (make-event "events" 'broadcast)
                 (make-event "that" 'broadcast)
                 (make-event "are" 'broadcast)
                 (make-event "more" 'broadcast)
                 (make-event "than" 'broadcast)
                 (make-event "twenty" 'broadcast)
                 (make-event "five" 'broadcast)
                 (make-event "in." 'broadcast)
                 (make-event "number" 'broadcast)
                 (make-event "testing" 'broadcast)
                 (make-event "if" 'broadcast)
                 (make-event "is" 'broadcast))
   "Updated events"))
(define (update-events events symbol msg)
  (if (= (length events) MAX-EVENTS)
      (append (list (make-event msg symbol)) (remove-last events))
      (append (list (make-event msg symbol)) events)))

; update-userlist : World ListOf<UserName> -> World
; Updates the userlist
; STRATEGY : Data decompostion on w : World
(begin-for-test 
  (check-equal? 
   (update-userlist
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) (list "ramesh" "suresh"))
   (make-world (make-editor "" "")
               (list "ramesh" "suresh")
               (list (make-event "set07 joined." 'join))
               "John")
   "Updated user list"))
(define (update-userlist w list-usernames)
  (make-world (world-editor w)
              (sort list-usernames string-case-insensitive<?)
              (world-events w)
              (world-client w)))

; display-error : World Message -> World
; Returns a world containing the error msg
; STRATEGY : Data decomposition on w: World
(begin-for-test 
  (check-equal? 
   (display-error
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) "hello")
   (make-world (make-editor "" "")
               (list "Jim" "Dan")
               (list
                (make-event "hello" 'error)
                (make-event "set07 joined." 'join))
               "John")
   "Displayed error"))
(define (display-error w msg)
  (make-world (world-editor w)
              (world-users w)
              (update-events (world-events w) 'error msg)
              (world-client w)))

; receive-private-msg : World Username Message -> World
; Handles private messages
; STRATEGY : Data decomposition on w : World
(begin-for-test 
  (check-equal? 
   (receive-private-msg
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) "Dan" "hello")
   (make-world (make-editor "" "")
               (list "Jim" "Dan")
               (list
                (make-event "< Dan > hello" 'private)
                (make-event "set07 joined." 'join))
               "John")
   "Private message received"))
(define (receive-private-msg w username msg)
  (make-world (world-editor w)
              (world-users w)
              (update-events (world-events w) 'private 
                             (string-append "< " username " > " msg))
              (world-client w)))

; receive-broadcast-msg : World Username Message -> World 
; Handles broadcast messaages
; STRATEGY : Data decomposition on w : World
(begin-for-test 
  (check-equal? 
   (receive-broadcast-msg
    (make-world (make-editor "" "")
                (list "Jim" "Dan") 
                (list (make-event "set07 joined." 'join)) 
                "John" ) "Dan" "hello")
   (make-world (make-editor "" "")
               (list "Jim" "Dan")
               (list
                (make-event "< Dan > hello" 'broadcast)
                (make-event "set07 joined." 'join))
               "John")
   "Broadcast message received"))
(define (receive-broadcast-msg w username msg)
  (make-world (world-editor w)
              (world-users w)
              (update-events (world-events w) 'broadcast 
                             (string-append "< " username " > " msg))
              (world-client w)))

; key-handler : World KeyEvent -> HandlerResult
; Handles keyboard user input
; STRATEGY : Data decomposition on ke : KeyEvent
(begin-for-test
  (check-equal? 
   (key-handler 
    (make-world (make-editor 
                  "hello"
                 "there")
                (list "Jack" "John") 
                (list (make-event "set07 joined." 'join))
                "John") 
    "left")
   (make-world (make-editor "hell" "othere")
               (list "Jack" "John")
               (list (make-event "set07 joined." 'join))
               "John")
   "World on left key event")
  (check-equal? 
   (key-handler 
    (make-world (make-editor 
                  "hello"
                 "there")
                (list "Jack" "John") 
                (list (make-event "set07 joined." 'join))
                "John") 
    "\r")
   (make-package
    (make-world (make-editor "" "")
                (list "Jack" "John")
                (list
                 (make-event
                  "< John > hellothere"
                  'broadcast)
                 (make-event "set07 joined." 'join))
                "John")
 (list 'broadcast "hellothere"))
   "World on enter key")
  (check-equal? 
   (key-handler 
    (make-world (make-editor 
                  "Ja"
                 "")
                (list "Jack" "John") 
                (list (make-event "set07 joined." 'join))
                "John") 
    "\t")
   (make-world (make-editor "Jack" "")
               (list "Jack" "John")
               (list (make-event "set07 joined." 'join))
               "John")
   "World on tab key"))
(define (key-handler w ke)
   (cond 
     [(string=? ke "\r") (send-message w)]
     [(string=? ke "\t") (prompt w)]
     [else 
      (other-key-pressed w ke)]))

; other-key-pressed : World KeyEvent -> World
; Computes the world if a key other than 'Tab' or 'Enter' was pressed
; STRATEGY : Data decomposition on w : World
(begin-for-test
  (check-equal? 
   (other-key-pressed  
    (make-world (make-editor 
                  "Ja"
                 "")
                (list "Jack" "John") 
                (list (make-event "set07 joined." 'join))
                "John") "left")
   (make-world (make-editor "J" "a")
               (list "Jack" "John")
               (list (make-event "set07 joined." 'join))
               "John")
   "World on left key"))
(define (other-key-pressed w ke)
  (make-world (edit (world-editor w) ke)
                  (world-users w)
                  (world-events w)
                  (world-client w)))
 
; send-message : World -> HandlerResult
; Computes the world after the user hits 'Enter'
; STRATEGY : Data decomposition on w : World
(begin-for-test
  (check-equal? (send-message 
                 (make-world 
                  (make-editor 
                   "Ja"
                   "")
                  (list "Jack" "John") 
                  (list (make-event "set07 joined." 
                                    'join))
                  "John"))
                (make-package
                 (make-world
                  (make-editor "" "")
                  (list "Jack" "John")
                  (list
                   (make-event "< John > Ja" 'broadcast)
                   (make-event "set07 joined." 'join))
                  "John")
                 (list 'broadcast "Ja"))
                "Message sent"))
(define (send-message w)
  (local ((define msg-type (return-msg-type w)))
    (make-package (make-world (make-editor "" "")
                              (world-users w)
                              (update-events (world-events w) msg-type
                                             (msg-assemble (world-editor w) 
                                                           msg-type
                                                           (world-client w)))
                              (world-client w))
                  (make-msg msg-type (world-editor w)))))

; make-msg : Symbol Editor -> MsgToServer
; Creates a message which is to be sent to the server
; STRATEGY : Function composition
(begin-for-test
  (check-equal? 
   (make-msg 'broadcast (make-editor "hello" "there"))
   (list 'broadcast "hellothere") 
    "message broadcasted")
  (check-equal? 
   (make-msg 'private (make-editor "hello:" "the"))
   (list 'private "hello" ":the") 
    "message is private"))
(define (make-msg msg-type ed)
  (if (check-type? 'broadcast msg-type)
      (list 'broadcast (editor->string ed))
      (local ((define target-name (extract-string (editor->string ed) COLON))
              (define pos-after-colon (string-length target-name)))
        (list 'private target-name 
              (substring  (editor->string ed) pos-after-colon)))))

; return-msg-type : World -> Symbol
; Returns 'private or 'broadcast depending on the message
; STRATEGY : Data decomposition on w : World
(begin-for-test
  (check-equal? (return-msg-type 
                 (make-world (make-editor  "hello:" "there")
                             (list "Jack" "John") 
                             (list (make-event "set07 joined."
                                               'join)
                                   (make-event "hello"
                                               'broadcast))
                             "John"))
                'private
                "Private message"))
(define (return-msg-type w)
  (local ((define editor-text (editor->string (world-editor w)))
          (define contains-colon? (string-contains? COLON editor-text)))
    (if (and contains-colon?
             (valid-username? (extract-string editor-text COLON)))
        'private
        'broadcast)))

; valid-username? : String -> Boolean
; Determines whether the given username is valid or not
; STRATEGY :Function composition
(begin-for-test
  (check-true (valid-username? "hi")
              "valid username")
  (check-false (valid-username? "How are you @")
               "invalid username"))
(define (valid-username? str)
  (and (< 1 (string-length str) 12)
       (only-letters-numbers str)))

; only-letters-numbers : String -> Boolean
; Returns true if all the characters in the given list contain 
; are either letters or numbers
; STRATEGY : Function composition
(begin-for-test
  (check-true (only-letters-numbers "hi")
              "valid username")
  (check-false (only-letters-numbers "How are you @")
               "invalid username"))
(define (only-letters-numbers str)
  (andmap (lambda (char) (or (char-numeric? char)
                              (char-alphabetic? char)))
          (string->list str)))

; extract-string : String 1String -> String
; Extracts the string before the seperator
; STRATEGY : Function composition
(begin-for-test 
  (check-equal? (extract-string "hello:" ":")
                "hello"
                "Extracted string before ':'"))
(define (extract-string str0 seperator)
  (local (;String 1String Natural -> Natural
          ;Returns the position of the seprator in str
          ;WHERE: pos-from-end is the position from the start of str
          ;STRATEGY: function composition
          (define (find-pos/a str seperator pos-from-start)
            (if (string=? (string-ith str pos-from-start) seperator)
                pos-from-start
                (find-pos/a str seperator (add1 pos-from-start)))))
    (substring str0 0 (find-pos/a str0 seperator 0))))

; msg-assemble : Editor Symbol UserName -> Message
; Creates a message to the specified client
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (msg-assemble (make-editor "hello:" "there") 'join "John")
                "< John->hello > there"
                "Printed a private message"))
(define (msg-assemble ed msg-type client)
  (if (check-type? 'broadcast msg-type)
      (string-append "< " client " > " (editor->string ed))
      (local ((define target-name (extract-string (editor->string ed) COLON))
              (define pos-after-colon (string-length target-name)))
        (string-append "< " client "->" 
                       target-name " > "
                       (substring  (editor->string ed) 
                                   (+ 1 pos-after-colon))))))

; prompt : World -> World
; Computes the world after the user hits 'Tab' 
; STRATEGY : Data decomposition on w : World
(begin-for-test
  (check-equal? 
   (prompt 
   (make-world (make-editor  "Ja" "")
               (list "Jack" "John") 
               (list (make-event "set07 joined."
                                 'join)
                     (make-event "hello"
                                 'broadcast))
               "John"))
   (make-world (make-editor "Jack" "")
               (list "Jack" "John")
               (list
                (make-event "set07 joined." 'join)
                (make-event "hello" 'broadcast))
               "John")
   "Prompted username"))
(define (prompt w)
  (make-world (prompt-username (world-editor w) 
                               (world-users w))
              (world-users w)
              (world-events w)
              (world-client w)))

; prompt-username : Editor Userlist -> Editor 
; Fetches the username if part of the name entered is a part of the chat
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (prompt-username (make-editor "Ja" "") 
                               (list "Jack" "John"))
                (make-editor "Jack" "")
                "Prompted to get Jack")
  (check-equal? (prompt-username (make-editor "" "") 
                               (list "Jack" "John"))
                (make-editor "" "")
                "Prompted to get nothing"))
(define (prompt-username ed userlist)
  (if (and (not (check-if-pre-empty ed))
           (check-if-post-empty ed))
      (update-editor ed userlist)
      ed))

; update-editor : Editor Userlist -> Editor
; Updates the editor so as to contain the prompted username
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (update-editor (make-editor "Ja" "") 
                               (list "Jack" "John"))
                (make-editor "Jack" "")
                "Prompted to get Jack")
  (check-equal? (update-editor (make-editor "X" "") 
                               (list "Jack" "John"))
                (make-editor "X" "")
                "Prompted to get nothing"))
(define (update-editor ed userlist)
  (local ((define pre (editor-pre ed))
          ;Editor ListOf<UserName> -> ListOf<UserName>
          ;Returns the list of matched user names.
          ;STRATEGY: function composition
          (define (get-username-matches ed userlist)
            (filter (lambda (x) (prefix? pre x)) userlist))
          (define matched-user-names (get-username-matches ed userlist)))
    (if (empty? matched-user-names)
        ed
        (make-editor (first matched-user-names) ""))))
          
; prefix? : String String -> Boolean
; Determins if a string 'pre' is a prefix of a string 'str'
; STRATEGY : Function composition
(begin-for-test
  (check-true (prefix? "Ja" "Jack")
              "Contains")
  (check-false (prefix? "X" "Jack")
              "Does not contains"))
(define (prefix? pre str)
  (and (>= (string-length str) (string-length pre))
       (string=? pre
                 (substring str 0 (string-length pre) ))))
 
; get-users : World -> ListOf<UserName>
; Returns a list of current chat participants, in lexicographic order.
; STRATEGY : Data decompostion on w: World
(begin-for-test
  (check-equal? 
   (get-users 
    (make-world (make-editor "hello" "there")
                (list "Jack" "John") 
                (list (make-event "set07 joined." 'join))
                "John"))
   (list "Jack" "John")
   "Fetched users"))
(define (get-users w)
  (world-users w)) 

; get-editor : World -> Editor
; Returns a representation of the chat client's input area.
; STRATEGY : Data decompostion on w: World
(begin-for-test
  (check-equal? 
   (get-editor
    (make-world (make-editor "hello" "there")
                (list "Jack" "John") 
                (list (make-event "set07 joined." 'join))
                "John"))
   (make-editor "hello" "there")
   "Fetched editor"))
(define (get-editor w)
  (world-editor w))
 
; get-editor-pre : Editor -> String
; get-editor-post : Editor -> String
; Returns an editor's content before and after the cursor.
; STRATEGY : Data decompostion on e: Editor
(begin-for-test
  (check-equal? 
   (get-editor-pre (make-editor "hello" "there"))
   "hello"
   "Fetched editor")
  (check-equal? 
   (get-editor-post (make-editor "hello" "there"))
   "there"
   "Fetched editor"))
(define (get-editor-pre e)
  (editor-pre e))
(define (get-editor-post e)
  (editor-post e))

; get-chat-history : World -> ListOf<String>
; Returns a list of chat events, rendered to string, 
; where each string format is the same as when the event is
; rendered to the chat window.(define MT (empty-scene 400 400))
; STRATEGY : Data decompostion on w: World
(begin-for-test
  (check-equal? (get-chat-history 
                 (make-world (make-editor 
                              "hello"
                              "there")
                             (list "Jack" "John") 
                             (list (make-event "set07 joined."
                                               'join)
                                   (make-event "hello"
                                               'broadcast))
                             "John"))
                (list "set07 joined." "hello")
                "Chat history found"))
(define (get-chat-history w)
  (local (;ListOf<Event> -> ListOf<Message>
          ;Extracts the messages from events
          ;STRATEGY: data decomposition lst-event : ListOf<Event>
          (define (events->msgs lst-event)
            (map (lambda (e) (event-msg e))
                 lst-event)))
  (events->msgs (world-events w))))

;Editor functions ----------------------------------

;print-pre : Editor -> Text
;Returns Text that contains the pre part of the Editor
(begin-for-test
  (check-equal? (print-pre (make-editor "hi" "there"))
                (text "hi" 12 "black")
                "Prints Text containing 'hi' "))
;STRATEGY : Data Decomposition on ed : Editor
(define (print-pre ed)
  (text (editor-pre ed) FONT-SIZE FONT-COLOR))

;print-post : Editor-> Text 
;Returns Text that contains the post part of the Editor
(begin-for-test
  (check-equal? (print-post (make-editor "hi" "there"))
                (text "there" 12 "black")
                "Prints Text containing 'there' "))
;STRATEGY : Data Decomposition on ed : Editor
(define (print-post ed)
  (text (editor-post ed) FONT-SIZE FONT-COLOR))

;render : Editor -> Image
;Returns an Image that contains a cursor between the pre and post parts 
; of the given Editor,
;on a empty-scene background
(begin-for-test
  (check-equal? (render-editor (make-editor "hi" "there"))
                (overlay/align "left" "center" 
                               (beside (text "hi" 12 "black") RECTANGLE 
                        (text "there" 12 "black")) EDITOR)
                "Prints Text containing 'hi' 'there' ")
  (check-true (image? 
               (render-editor (make-editor "this is a dummy test case to show
                that this exceeds the width of
                an editor. Does this work?" "")))))
;STRATEGY : Function Composition
(define (render-editor ed) 
  (overlay/align "left" "center"
                 (if (no-overflow? ed)
                     (beside (print-pre ed) RECTANGLE (print-post ed))
                     (beside (text (str-within-view ed) FONT-SIZE FONT-COLOR)
                             RECTANGLE))
                 EDITOR))

; str-within-view : Editor -> String
; Shows only those characters that should be in view, if overflow occurs
(begin-for-test
  (check-true (string? (str-within-view 
                        (make-editor "this is a dummy test case to show
                          that this exceeds the width of an editor. 
                          Does this work?" "")))))
;STRATEGY : data decomposition on ed : Editor
(define (str-within-view ed)
  (local ((define str-pre (editor-pre ed)))
    (substring str-pre (find-pos-in-pre str-pre MSG-WIDTH))))

; find-pos-in-pre: String Natural -> Natural
; Returns the position of pre at which point the image of it's end 
; is less than the given width.
; STRATEGY: function composition
(define (find-pos-in-pre pre0 width)
  (local (;String Natural Natural -> Natural
          ;Returns the position of pre at which point the image of it's end 
          ;is less than the given width.
          ;WHERE: pos-from-end is the position from end of the pre0
          ;STRATEGY: function composition
          (define (find-pos-in-pre/a pre width pos-from-end)
            (if (or (= pos-from-end 0)
                    (> (image-width (string->img 
                                     (substring pre (sub1 pos-from-end))))
                       width))
                pos-from-end
                (find-pos-in-pre/a pre width (sub1 pos-from-end)))))
    (find-pos-in-pre/a pre0 width (string-length pre0))))

; ;string->img : String -> Image
; Converts a string to an image
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (string->img "a")
                (text "a" 12 "black")
                "Converted into a img"))
(define (string->img s)
  (text s FONT-SIZE FONT-COLOR))

;pre-ke-appender : Editor KeyEvent -> String
;Appends the given Key to the pre part of the given Editor
(begin-for-test
  (check-equal? (pre-ke-appender (make-editor "hi" "there") "a")
                "hia"
                "Appends 'a' to 'hi'"))
;STRATEGY : Data Decomposition on ed : Editor
(define (pre-ke-appender ed ke)
  (string-append (editor-pre ed) ke))

;add-key : Editor KeyEvent -> Editor
;The function appends the given Key to the pre part of given Editor,
;IF the length of the Editor has not overflown; otherwise preserves the
; previous state of the Editor
;STRATEGY : Data Decomposition on ed : Editor
(define (add-key ed ke)
  (make-editor (pre-ke-appender ed ke) (editor-post ed)))

;backspace : Editor -> String
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
  (if (check-if-pre-empty ed)
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

;pre-string-last : Editor -> String
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

;post-string-appender : Editor -> String
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

;post-string-remove-first : Editor -> String
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

;post-string-rest : Editor -> String
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

;pre-string-appender : Editor -> String
;The function returns the pre part of the given Editor, after suffixing it with 
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
(begin-for-test
  (check-true (no-overflow? (make-editor "hi" ""))
              "Overflow occurs")
  (check-false (no-overflow? (make-editor "this is a dummy test case to show
                                           that this exceeds the width of an 
                                           editor. Let's see if it works" ""))
              "Overflow does not occur"))
;STRATEGY : Function composition
(define (no-overflow? ed)
 (< (image-width (print-pre ed)) EDITOR-WIDTH))

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
                "Right was pressed. Moves cursor one character to the right")
  (check-equal? (edit (make-editor "hi" "there") "up")
                (make-editor "hi" "there")
                "Up was pressed."))
;STRATEGY : Data Decomposition on ke : KeyEvent
(define (edit ed ke) 
   (cond [(string=? ke "\u007F") ed]
         [(string=? ke "\b") (move-back ed)]
         [(= (string-length ke) 1) (add-key ed ke)]
         [(string=? ke "left") (move-left ed)]
         [(string=? ke "right") (move-right ed)]
         [else ed]))

; editor-> string : Editor -> String
; Converts the Editor into a String
; STRATEGY : data decomposition on e : Editor
(begin-for-test 
  (check-equal? (editor->string (make-editor "hi" "there"))
                "hithere"
                "Converted into a string"))
(define (editor->string e)
  (string-append (editor-pre e) (editor-post e)))



