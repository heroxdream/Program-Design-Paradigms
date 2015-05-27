;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname in-class) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
; An PegLabeling is a (make-pegs Peg Peg Peg)
; Represents a labeling of the physical Hanoi pegs (left, middle, right)
; as logical "source", "target", or "helper" pegs.
(define-struct pegs (src tgt help))
; Template
(define (pegs-fn peglabels)
  (...(peg-fn (peg-src))... 
   ...(peg-fn (peg-tgt))... 
   ...(peg-fn (peg-help))...))
 
; A Peg is one of:
; - LEFT
; - MIDDLE
; - RIGHT
; Represents a physical Hanoi peg.
; Template
(define (peg-fn peg)
  (cond 
    [(left? peg) ...]
    [(middle? peg)...]
    [(right? peg) ...]))

(define (left? peg)...)
(define (right? peg)...)
(define (middle? peg)...)

; A Move is a (make-move Peg Peg)
(define-struct move (from to))
; Represents the movement of a the top Disc from the first peg to the second.
; Template
(define (move-fn m)
  (...(peg-fn (move-from m))...
   ...(peg-fn (move-to m)...)))

 
; solve-hanoi : PegLabeling Natural -> ListOf<Move>
; Computes the moves required to relocate n stacked Hanoi discs, 
; from (pegs-src ps) to (pegs-tgt ps)
; Strategy: ???
(define (solve-hanoi pegs0 n)
  (local (
          (define (solve-hanoi/a pegs n moves-so-far)
            (cond 
              [(zero? n) moves-so-far]
              [else (append 
                     (solve-hanoi/a (make-pegs (pegs-src pegs) 
                                               (pegs-help pegs)
                                               (pegs-tgt pegs))
                                  (sub1 n)
                                  moves-so-far)
                     (cons (make-move (pegs-src pegs) (pegs-tgt pegs))
                           (solve-hanoi/a (make-pegs (pegs-help pegs) 
                                                     (pegs-tgt pegs)
                                                     (pegs-src pegs))
                                        (sub1 n) moves-so-far)))]))
          )
    (solve-hanoi/a pegs0 n '())))







