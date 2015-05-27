;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-avail) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")
(require rackunit)
(require racket/set)
(require "sched-general.rkt")


(provide schedule/avail-ok?)
(provide schedule/avail)
(provide avg-choice)

; schedule/avail-ok? : StudentAvails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; Strategy: Function composition
; Example:
(begin-for-test
  (check-true (schedule/avail-ok? STUDENTS 
                                  (list
                                   (make-codewalk 't6 (list 'rick 'theo) 2)
                                   (make-codewalk 't3 (list 'ali 'pai 'sud) 3)
                                   (make-codewalk 't5 (list 'hank) 2)
                                   (make-codewalk 't2 (list 'james) 1)
                                   (make-codewalk 't1 (list 'han 'xuan) 2)
                                   (make-codewalk 't2 empty 0)
                                   (make-codewalk 't4 empty 0)))              
              "ok"))
(define (schedule/avail-ok? students cws)
  (check-for-avail/unavail-ok? students cws true))

; schedule/avail : StudentAvails CodeWalks -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Strategy: Function composition
; Example:
(begin-for-test
  (check-equal? (schedule/avail STUS CWS)
                (list
                 (make-codewalk 't1 (list 'han 'xuan) 2)
                 (make-codewalk 't2 (list 'pai) 1)
                 (make-codewalk 't3 (list 'sud) 3)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't4 empty 0))
                "Test failed. schedule/avail should return a codewalk")
  (check-false (schedule/avail (list (make-student 'jack '(t1 t2))) '())))
(define (schedule/avail students cws) 
  (local((define schedualable? 
           (not (boolean? (check-for-avail/unavail students cws true))))
         ;sched-on-permutation: StudentAvails CodeWalks -> ListOf<CodeWalks>
         ;Scheduals codewalks on a permutation list of students
         ;Strategy: function composition
         (define (sched-on-permutation students cws)
           (sort 
            (map (lambda (stus) 
                   (local ((define result (check-for-avail/unavail students cws 
                                                                   true)))
                     (list (avg-choice students result) result)))
                 (permutation/sort students))
            (lambda (s1 s2) (< (first s1) (first s2))))))
    (if schedualable?
        (second (first (sched-on-permutation students cws)))
        #false)))

; avg-choice : StudentAvails CodeWalks -> PosReal
; Calculates the average schedued preferences of the given students.
; WHERE: (schedule/avail-ok? students cws) = #t
; Strategy: function composition
(begin-for-test
  (check-equal? (avg-choice  (list
                              (make-student 'xuan (list 't2 't1))
                              (make-student 'sud (list 't1)))
                             (list (make-codewalk 't1 (list 'xuan 'sud) 2)))
                1.5))
(define (avg-choice students cws)
  (/ (foldr + 0
            (map (lambda (cw) (score cw students))
                 cws))
     (length students)))

; CodeWalk StudentAvails -> PosReal
; Calculate the total schedued preference of the given CodeWalk cw.
; Strategy: data decomposition of cw : CodeWalk
(begin-for-test
  (check-equal? (score (make-codewalk 't1 (list 'xuan 'sud) 2) 
                       (list
                        (make-student 'xuan (list 't2 't1))
                        (make-student 'sud (list 't1))))
                3
                "total preference score is 3"))
(define (score cw students)
  (local((define t (codewalk-time cw))
         (define ids (codewalk-students cw))
         ;get-id-prefs : StudentID -> ListOf<Time>
         ;Returns the student's preference time list from student
         ;by the given studentID id.
         ;Strategy: function composition
         (define (get-id-prefs id)
           (student-prefs 
            (first 
             (filter (lambda 
                         (stu) 
                       (symbol=? id 
                                 (student-id stu)))
                     students)))))
    (foldr + 0
           (map (lambda (id) 
                  (index t
                         (get-id-prefs id)))
                ids))))

; index: Time ListOf<Time> -> Natural
; Returns the 1-based position of key in list array
; WHERE: (member? key array) = #true
; STRATEGY : Data decomposition on array : ListOf<Time>
(begin-for-test
  (check-equal? (index 'x (list 'x 'y))
                1
                "Computed index for 'x")
  (check-equal? (index 'z '())
                -1))
(define (index key array)
  (cond
    [(empty? array) -1]
    [else (if (eq? key (first array))
              1
              (add1 (index key (rest array))))]))

; permutation/sort: StudentAvails -> ListOf<StudentAvails>
; Returns the unique permutation of StudentAvails lst after sorting by
; length of prefs.
; Strategy: function composition
(begin-for-test
  (check-equal? (permutation/sort (list STU1 STU2))
                (list
                 (list
                  (make-student 'xuan (list 't1 't2))
                  (make-student 'pai (list 't2 't3)))
                 (list
                  (make-student 'pai (list 't2 't3))
                  (make-student 'xuan (list 't1 't2))))))
(define (permutation/sort lst)
  (set->list (list->set (map (lambda (l) (sort-by-prefs l))
                             (permutation lst)))))

; permutation: X -> ListOf<X>
; Returns the permuation of StudentAvails
; STRATAGY: data decomposition on sas : StudentAvails
(begin-for-test
  (check-equal? (permutation (list 'a 'b 'c))
                (list
                 (list 'a 'b 'c)
                 (list 'b 'a 'c)
                 (list 'b 'c 'a)
                 (list 'a 'c 'b)
                 (list 'c 'a 'b)
                 (list 'c 'b 'a))))
(define (permutation lst) 
  (cond
    [(empty? lst) (list '())]
    [else (insert/list (first lst)
                       (permutation (rest lst)))]))

; insert/list: X ListOf<ListOf<X>> -> ListOf<ListOf<X>>
; Insert element e into each elements of lst
; STRATAGY: data decomposition on lst : ListOf<ListOf<X>>
(begin-for-test
  (check-equal? (insert/list 'a '((b) (c)))
                (list (list 'a 'b) (list 'b 'a) (list 'a 'c) (list 'c 'a))))
(define (insert/list e lst) 
  (cond
    [(empty? lst) '()]
    [else (append (insert e '() (first lst))
                  (insert/list e (rest lst)))]))

; insert: X ListOf<X> ListOf<X> ->  ListOf<ListOf<X>>
; Insert elements e into each place of lst
; STRATAGY: data decomposition on lst : ListOf<X>
(begin-for-test
  (check-equal? (insert 'a '(c) '(b))
                (list (list 'c 'a 'b) (list 'c 'b 'a))))
(define (insert e head lst)
  (cond
    [(empty? lst) (list (append head (list e)))]
    [else (cons (append head (list e) lst) 
                (insert e 
                        (append head (list (first lst)))
                        (rest lst)))]))