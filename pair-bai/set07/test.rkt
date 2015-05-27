;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))

(list "((1 * 2)"
      " +"
      " (+ 3 4))")




 (list "(10"
       " +"
       " 20"
       " +"
       " 30)")
 
 
(list "(1 + 2 + 3)")


(list "((1"
      "  *"
      "  2)"
      " +"
      " (+ 3"
      "    4))") 


(+ 22 333 44)



(define str "01234")


;NEListOf<String> -> String
(define (stack->oneline lst)
  (local (;
          ;
          (define (stack->oneline/a lst a)
            (cond
              [(empty? (rest lst)) (string-append a (first lst))]
              [else (stack->oneline/a (rest lst) 
                                      (string-append a (first lst)))])))
    (stack->oneline/a lst "")))

