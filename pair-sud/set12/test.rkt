#lang racket
(require lang/posn)


(define dynamic (make-posn 1 1))
(define center (make-posn 0 0))
(define (radians-c/d dynamic center)
  (atan (/ (- (posn-x dynamic) (posn-x center))
           (- (posn-y dynamic) (posn-y center)))))

(radians-c/d dynamic center)