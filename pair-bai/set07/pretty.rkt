;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")

(require rackunit)

(define TIME-ON-TASK 20)

(provide expr->strings)

;Data definition --------------

(define-struct add (exprs infix?))
; A Add is a (make-add NEListOf<Expr> Boolean)
; INTERP: the list of Expr represents the exprs that add will operate on, and
;         the Boolean infix? indicates whether the expression uses infix or
;         prefix
(define a1 (make-add (list 3 4) false))

; Template:
; add-fn : Add -> ???
; STRATEGY : Data decomposition on a : Add
(define (add-fn a)
  (...(nelist-fn (add-exprs a))...
   ...(add-infix? a)...))

(define-struct mul (exprs infix?))
; A Mul is a (make-mul NEListOf<Expr> Boolean)
; INTERP: the list of Expr represents the exprs that mul will operate on, and
;         the Boolean infix? indicates whether the expression uses infix or
;         prefix
(define m1 (make-mul (list 3 4) false))

; Template:
; mul-fn : Mul -> ???
; STRATEGY : Data decomposition on m : Mul
(define (mul-fn m)
  (...(nelist-fn (mul-exprs m))...
   ...(mul-infix? m)...))

; An Expr is one of:
; - Integer
;  - (make-add (cons Expr NEListOf<Expr>) Boolean)
;  - (make-mul (cons Expr NEListOf<Expr>) Boolean)
; Represents a numeric expression that is either an integer, or an addition
; or multiplication of two or more expressions.
; The Boolean flag indicates whether the expression uses infix or prefix 
; notation.

(define EXAMPLE
  (make-add (list (make-mul (list 1 2) true)
                  (make-add (list 3 4) false))
            true))

;TEMPLATE :
; expr-fn :Expr -> ???
; STRATEGY : Data decomposition on ex : Expr
;(define (expr-fn ex)
;  (cond
;    [(integer? ex) ...]
;    [(add? ex) (...(add-fn ex)...)]
;    [(mul? ex) (...(mul-fn ex)...)]))

; Functions begin -----------------

; expr->strings : Expr Natural -> ListOf<String>
; Returns a rendering of exp as a sequence of lines,
; where each line is a string not longer than width characters.
; EFFECT: errors if expr cannot fit width
(begin-for-test
  (check-equal? (expr->strings EXAMPLE 100)
                (list "((1 * 2) + (+ 3 4))")
                "unstacked")
  (check-equal? (expr->strings EXAMPLE 9)
                (list "((1 * 2)"
                      " +"
                      " (+ 3 4))")
                "one level stacking")
  (check-equal? (expr->strings EXAMPLE 7)
                (list "((1"
                      "  *"
                      "  2)"
                      " +"
                      " (+ 3"
                      "    4))")
                "two level stacking")
  (check-equal? (expr->strings EXAMPLE 8)
                (list "((1 * 2)" 
                      " +" 
                      " (+ 3" 
                      "    4))")
                "two level stacking"))
; STRATEGY : function composition
(define (expr->strings expr width)
  (expr->strings/layer expr width 0))


; expr->strings/layer : Expr Natural Natural -> ListOf<String>
; Returns a rendering of exp as a sequence of lines,
; where each line is a string not longer than width characters.
; EFFECT: errors if expr cannot fit width
; STRATEGY : Data decomposition on expr : Expr
(define (expr->strings/layer expr width layer)
  (cond 
    [(integer? expr) (integer->list expr width)]
    [(add? expr) (expression->list + (add-exprs expr) 
                                   (add-infix? expr) width layer)]
    [(mul? expr) (expression->list * (mul-exprs expr) 
                                   (mul-infix? expr) width layer)]))

; Integer Natural -> NEListOf<String>
; Renders the integer as  a list if the integer can 
; be fitted within the given width
; EFFECT: errors if expr cannot fit width
; STRATEGY : Function compostion
(begin-for-test
  (check-equal? (integer->list 1 2)
                '("1")
                "")
  (check-error (integer->list 100 2)
                "expr cannot fit width")
  (check-equal? (integer->list 1000 4)
                '("1000")
                ""))
(define (integer->list expr width)
  (local ((define str-of-number (number->string expr)))
    (if (> (string-length str-of-number) width)
        (error "expr cannot fit width")
        (list str-of-number))))

; expression->list : [X X -> X] NEListOf<Expr> Boolean -> NEListOf<String>
; Generates a non-empty list of string depending on whether it is a infix or a
; prefix expression
; STRATEGY : Function composition 
(define (expression->list operator exprs infix? width l)
  (if infix?
      (expression->list-infix operator exprs width l)
      (expression->list-prefix operator exprs width l)))

; expression->list-infix: [X X -> X] NEListOf<Expr> Natural Natural -> 
; NEListOf<String>
; Generates a non-empty list of string as an infix expression
; STRATEGY : Function composition
(define (expression->list-infix operator exprs width l)
  (local ((define nel-nel-str (expr->nelist-nelist-string exprs width 1 l))
          (define infix-multi-line (infix-multi-line? nel-nel-str width l)))
    (if infix-multi-line
        (infix-stack-layout nel-nel-str operator)
        (infix-oneline-layout nel-nel-str operator))))

; expression->list-prefix:[X X -> X] NEListOf<Expr> Natural Natural -> 
; NEListOf<String>
; Generates a non-empty list of string as a prefix expression
; STRATEGY : Function composition
(define (expression->list-prefix operator exprs width l)
  (local ((define nel-nel-str (expr->nelist-nelist-string exprs width 3 l))
          (define prefix-multi-line (prefix-multi-line? nel-nel-str width l)))
    (if prefix-multi-line
        (prefix-stack-layout nel-nel-str operator)
        (prefix-oneline-layout nel-nel-str operator))))

; expr->nelist-nelist-string :
; NEListOf<Expr> Natural Natural Natural Natural -> NEListOf<NEListOf<String>>
; Produces a non-empty list of non-empty list of strings
; STRATEGY : Function composition 
(define (expr->nelist-nelist-string exprs width reduce l)
  (local((define last-expr (first (reverse exprs)))
         (define rest-expr (reverse (remove last-expr (reverse exprs))))
         ; assemble-lst : Natural -> ListOf<ListOf<String>>
         ; Produces a list of list of strings
         ; STRATEGY : Function composition
         (define (assemble-lst width)
           (append (map (lambda (expr) (expr->strings expr (- width reduce)))
                        rest-expr)
                   (last-expr->strings last-expr width reduce l))))
    (assemble-lst width)))

; last-expr->strings :
; Expr Natural Natural Natural -> NEListOf<NEListOf<String>>
; Produces a non-empty list of non-empty list of strings for the last expr
; STRATEGY : data decomposition on last-expr : Expr
(define (last-expr->strings last-expr width reduce l)
  (cond
    [(integer? last-expr) 
     (list 
      (expr->strings last-expr (- width (+ reduce (add1 l)))))]
    [else 
     (list 
      (expr->strings/layer last-expr (- width reduce)
                           (add1 l)))]))

; infix-stack-layout : NEListOf<NEListOf<String>> [X X -> X] -> NEListOf<String>
; Generates an infix expression in the form of a stack layout
; STRATEGY : Function composition
(define (infix-stack-layout nel-nel-str operator)
  (local(
         (define flatened-lst (add-space 
                               (infix-flat-lst nel-nel-str operator) 1))
         ; replace-first-space-with-paranthesis :
         ; NEListOf<String> -> NEListOf<String>
         ; Replaces the first space in the first of list with paranthesis
         ; STRATEGY : Function composition
         (define (replace-first-space-with-parenthesis strings)
           (local (
                   (define first-string (first strings))
                   (define new-string 
                     (string-append "("
                                    (substring first-string 1)))
                    ; replace :
                    ; NEListOf<String> -> NEListOf<String>
                    ; Appends "(" to the list of strings after removing
                    ; the first item in the list
                    ; STRATEGY : Function composition 
                   (define (replace lstOfstrings)
                     (cons new-string (remove first-string lstOfstrings))))
             (replace strings))))
    (add-last-parenthesis (replace-first-space-with-parenthesis flatened-lst))))

; prefix-stack-layout :NEListOf<NEListOf<String>> [X X -> X] -> NEListOf<String>
; Generates a prefix expression in the form of a stack layout
; STRATEGY : Function composition
(define (prefix-stack-layout nel-nel-str operator)
  (local(
         (define flatened-lst (add-space (prefix-flat-lst nel-nel-str) 3))
         ; replace-first-space-with-paranthesis :
         ; NEListOf<String> -> NEListOf<String>
         ; Replaces the first two spaces in the first of list with paranthesis
         ; STRATEGY : Function composition
         (define (replace-first-two-space-with-parenthesis-and-oper strings)
           (local (
                   (define first-string (first strings))
                   (define new-string 
                     (string-append "(" 
                                    (operator->string operator)
                                    (substring first-string 2)))
                    ; replace :
                    ; NEListOf<String> -> NEListOf<String>
                    ; Appends "(" to the list of strings after removing
                    ; the first item in the list
                    ; STRATEGY : Function composition 
                   (define (replace lstOfstrings)
                     (cons new-string (remove first-string lstOfstrings))))
             (replace strings))))
    (add-last-parenthesis 
     (replace-first-two-space-with-parenthesis-and-oper flatened-lst))))

; add-last-paranthesis : NEListOf<String> -> NEListOf<String>
; Adds a closing paranthesis ')' at the end of the list
; STRATEGY : Function composition
(define (add-last-parenthesis strings)
  (local ((define last-string (first (reverse strings)))
          (define new-string (string-append last-string ")"))
          ;add-str : NEListOf<String> -> NEListOf<String>
          ;Creates a new list after appending ')' at the end
          ;STRATEGY : Function compostion
          (define (add-str lstOfstrings)
            (append (reverse (remove last-string (reverse lstOfstrings)))
                    (list new-string))))
    (add-str strings)))

; infix-flat-lst : NEListOf<NEListOf<String>> [X X -> X] -> NEListOf<String>
; Converts the list of list of string into a flat list of string
; after inserting the operators within the expression
; STRATEGY : Function composition 
(define (infix-flat-lst nel-nel-str operator)
  (local (
          (define operator-str (operator->string operator))
          ; add-operator : NEListOf<NEListOf<String>> -> NEListOf<String>
          ; Adds the operator within the expression
          ; STRATEGY : Function composition
          (define (add-operator nel-nel-str)
            (foldr (lambda (curr-lst rest-lst) 
                     (append curr-lst
                             (list operator-str)
                             rest-lst))
                   '()
                   nel-nel-str))
          ; remove-last-oper : NEListOf<String> -> NEListOf<String>
          ; Removes the operator at the end of the expression
          ; STRATEGY : Function composition
          (define (remove-last-oper lst)
            (reverse (remove (first (reverse lst))
                    (reverse lst)))))
    (remove-last-oper (add-operator nel-nel-str))))

; infix-flat-lst : NEListOf<NEListOf<String>> -> NEListOf<String>
; Converts the list of list of string into a flat list of string
; STRATEGY : Function composition 
(define (prefix-flat-lst nel-nel-str)
  (local (; NEListOf<NEListOf<String>> -> NEListOf<String>
          ; Converts the list of list of string into a flat list of string
          ; STRATEGY : Function composition 
          (define (flat nel-nel-str)
            (foldr (lambda (curr-lst rest-lst) 
                     (append curr-lst rest-lst))
                   '()
                   nel-nel-str))) 
    (flat nel-nel-str)))

; add-space : NEListOf<String> NonNegInt -> NEListOf<String>
; Adds 'space-count' number of spaces to every item in the list of strings.
; STRATEGY : Function composition
(define (add-space strings space-count)
  (local ((define space-str (space-string space-count))
          (define (add-space lst)
            (map (lambda (e) (string-append space-str e))
                 lst)))
    (add-space strings)))

; space-string : NonNegInt -> String
; Produces 'count' number of spaces
; STRATEGY : Function compostion
(define (space-string count)
  (make-string count #\ ))

; infix-oneline-layout: NEListOf<NEListOf<String>> [X X -> X] ->NEListOf<String>
; Produces a one line layout of an infix expression
; STRATEGY : Function composition
(define (infix-oneline-layout nel-nel-str oper)
  (local ((define stack-strings (infix-stack-layout nel-nel-str oper)))
    (list (stack->oneline stack-strings))))

;prefix-oneline-layout: NEListOf<NEListOf<String>> [X X -> X] ->NEListOf<String>
; Produces a one line layout of a prefix expression
; STRATEGY : Function compostion
(define (prefix-oneline-layout nel-nel-str oper)
  (local ((define stack-strings (prefix-stack-layout nel-nel-str oper))
          ; remove-redundant-space : String NEListOf<String>
          ; Removes all the additional spaces 
          ; STRATEGY : Function compostion
          (define (remove-redundant-space head strings)
            (cons head (map (lambda (s) (substring s 2))
                 strings))))
    (list (stack->oneline (remove-redundant-space (first stack-strings)
                                                  (rest stack-strings))))))

; stack->oneline : NEListOf<String> -> String
; Converts the stack layout into one line layout of an expression
; STRATEGY : Function compostion
(define (stack->oneline lst0)
  (local (;NEListOf<String> String -> String
          ;Assembles list of strings to one string
          ;WHERE: builded-str-so-far is the string of lst0 
          ;builded so far, and lst is the list of strings 
          ;to be builded
          ;STRATEGY : Data decomposition on lst : NEListOf<String>
          (define (stack->oneline/a lst builded-str-so-far)
            (cond
              [(empty? (rest lst)) 
               (string-append builded-str-so-far (first lst))]
              [else (stack->oneline/a (rest lst) 
                                      (string-append builded-str-so-far 
                                                     (first lst)))])))
    (stack->oneline/a lst0 "")))

; infix-multi-line? NEListOf<NEListOf<String>> Natural Natural -> Boolean
; Determines whether an infix expression has to be rendered on one line
; or multiple lines
; STRATEGY : Function composition
(define (infix-multi-line? nel-nel-str width l)
  (or (more-than-one-element? nel-nel-str)
      (infix-beyond-limit? nel-nel-str width l)))

; infix-multi-line? NEListOf<NEListOf<String>> Natural Natural -> Boolean
; Determines whether a prefix expression has to be rendered on one line
; or multiple lines
; STRATEGY : Function composition
(define (prefix-multi-line? nel-nel-str width l)
  (or (more-than-one-element? nel-nel-str)
      (prefix-beyond-limit? nel-nel-str width l)))

; infix-beyond-limit? : NEListOf<NEListOf<String>> Natural Natural -> Boolean
; Determines whether an infix expression is going beyond limit
; STRATEGY : Function composition
(define (infix-beyond-limit? nel-nel-str width l)
  (local((define length-of-oper/spaces 
           (+ 2 (* 3 (- (length nel-nel-str) 1)))))
    (beyond-limit? length-of-oper/spaces nel-nel-str width l)))

; prefix-beyond-limit? NEListOf<NEListOf<String>> Natural Natural -> Boolean
; Determines whether a prefix expression is going beyond limit
; STRATEGY : Function composition
(define (prefix-beyond-limit? nel-nel-str width l)
  (local((define length-of-oper/spaces 
           (+ 3 (length nel-nel-str))))
    (beyond-limit? length-of-oper/spaces nel-nel-str width l)))

; beyond-limit? Natural NEListOf<NEListOf<String>> Natural Natural -> Boolean
; Determines whether an expression is going beyond limit
; STRATEGY : Function composition
(define (beyond-limit? length-of-oper/spaces nel-nel-str width l)
  (> (+ l length-of-oper/spaces (length-of-elements nel-nel-str)) 
     width))

; lenght-of-elements : NEListOf<NEListOf<String>> -> Natural
; Returns the number of characters the expression occupies including spaces
; STRATEGY : Function composition
(define (length-of-elements nel-nel-str)
  (foldr (lambda (lst rest-length) 
                    (+ rest-length 
                       (string-length (first lst))))
                  0
                  nel-nel-str))

; more-than-one-element? : NEListOf<NEListOf<String>> -> Boolean
; Returns true if list of list of strings has one list of string that 
; contains more than one element 
; STRATEGY : Function composition
(define (more-than-one-element? nel-nel-str)
  (local (;NEListOf<NEListOf<String>> -> Boolean
          ; Returns true if list of list of strings has one list of string that 
          ; contains more than one element 
          ; STRATEGY : Function composition
          (define (check lstoflst)
            (ormap (lambda (lst) (> (length lst) 1))
                   lstoflst)))
    (check nel-nel-str)))

; operator->string : [X X -> X] -> String
; Converts the + to "+", * to "*"
; STRATEGY : Function composition
(define (operator->string oper)
  (if (eq? + oper)
      "+"
      "*"))