;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
'(1 2 3 4 5 6)
'(t f t f f)
'(1 2 4 6 72 1)

(define (arith/bool-operate op low hi lst)
  (local ((define mid (floor (/ (+ low hi) 2))))
    (if (= low hi)
        (list-ref lst hi)
        (op (arith/bool/operate op low mid lst)
            (arith/bool/operate op (add1 mid) hi lst)))))

(define and-op
  (lambda (x y) (and x y)))

(define or-op
  (lambda (x y) (or x y)))

(define (cmp-operate op low hi lst)
  (local ((define mid (floor (/ (+ low hi) 2))))
    (if (= (- hi low) 1)
        (op (list-ref lst low) (list-ref lst hi))
        (and (com/operate op low mid lst)
             (com/operate op mid hi lst)))))



