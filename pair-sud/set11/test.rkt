#lang racket
(require 2htdp/image)
(define Animal<%>
  (interface ()
    ; roam : -> String
    ; RETURNS: ...
    roam
    ; eat : -> String
    ; RETURNS: ...
    eat
    ; vocalize : -> String
    ; RETURNS: ...
    vocalize
    ; sleep : -> String
    ; RETURNS: ...
    sleep))

(define Sheep%
  (class* object% (Animal<%>)
    (init-field name age color)
    (define/public (set-name new-name)
      (set-field! name this new-name))
    (define/public (set-age new-age)
      (set-field! age this new-age))
    (define/public (set-color new-color)
      (set-field! color this new-color))
    
    ; INTERPRETATION: 'name' is the sheep's personal name (e.g. "Dolly"),
    ;   'age' is its age in years since birth, and 'color' is the
    ;   color of its coat, e.g. 'black, 'gray, 'silver, 'brown, 'red,
    ;   or 'moorit.
 
    ; roam : -> String
    ; ...
    (define/public (roam)
      (string-append "I am a roaming sheep named " name))
 
    ; eat : -> String
    ; ...
    (define/public (eat food)
      (string-append "Munch munch " food))
 
    ; vocalize : -> String
    ; ...
    (define/public (vocalize)
      (wawa "Baa baa"))
    
    (define/public (clone new-name)
      (send* this
        (set-name "H")
        (set-age 70)
        (set-color "pink")))
 
    ; sleep : -> String
    ; ...
    (define/public (sleep)
      "Zzzzzzz")
    
    (define/public (to-string)
      (string-append name (number->string age) (symbol->string color)))
    
    (super-new)))

(define (wawa s)
  s)

(define sheep1 (new Sheep% [name "Dolly"] [age 7] [color 'red]))