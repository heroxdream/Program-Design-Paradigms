;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-general) (read-case-sensitive #t) (teachpacks ((lib "arrow-gui.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "arrow-gui.rkt" "teachpack" "htdp")))))
(require "extras.rkt")
(require rackunit)
(require racket/set)
(define TIME-ON-TASK 0)

(provide (all-defined-out)) 

; A CodeWalks is a ListOf<CodeWalk>
; Represents a codewalk schedule.

; A Time is a Symbol
; Represents a day of week and time.
(define TUE135 '1:35pmTues)
(define TUE325 '3:25pmTues)

; A CodeWalk is a (make-codewalk Time ListOf<StudentID> PosInt)
; Represents a codewalk with time, assigned students, and a max capacity.
(define-struct codewalk (time students max))
(define CW-TUE135 (make-codewalk TUE135 empty 1))
(define CW-TUE325 (make-codewalk TUE325 empty 1))
; Template:
; cw-fn : CodeWalk -> ???
; Strategy: data decomposition on cw : CodeWalk
(define (cw-fn cw)
  (...(codewalk-time cw)
      ...(list-fn (codewalk-students cw))
      ...(codewalk-max cw)...))

; A StudentID is a Symbol
; Represents a student (or pair) via their ccs ID(s).
(define sid1 'xuan)

; A Preferences is a ListOf<Time>
; Represents a list of code walk times.

; A Student is one of:
; - StudentUnavail
; - StudentAvail

(define-struct student (id prefs))
; A StudentAvail is a (make-student StudentID Preferences)
; Represents a student and their available times, most-preferred first.
; An unlisted time means the student is unavailable.
(define stu1 (make-student sid1 '()))
; Template:
; stu-avail-fn : StudentAvail -> ???
; Strategy: data decomposition on stu : StudentAvail
(define (stu-avail-fn stu)
  (...(student-id stu)...(list-fn (student-prefs stu))))

; A StudentUnavail is a (make-student StudentID Preferences)
; Represents a student and their unavailable times.
(define stu2 (make-student sid1 '()))
; Template:
; stu-unavail-fn : StudentUnavail
; Strategy: data decomposition on stu : StudentUnavail
(define (stu-unavail-fn stu)
  (...(student-id stu)...(list-fn (student-prefs stu))))

; A StudentAvails is a ListOf<StudentAvail>
; WHERE: there are no duplicate StudentIDs

; A StudentUnavails is a ListOf<StudentUnavail>
; WHERE: there are no duplicate StudentIDs

; A Students is one of:
; - StudentUnavails
; - StudentAvails
; WHERE: there are no duplicate StudentIDs

; Examples:
(define XUAN 'xuan)
(define PAI 'pai)
(define SUD 'sud)
(define HAN 'han)

(define T1 't1)
(define T2 't2)
(define T3 't3)
(define T4 't4)
(define T5 't5)
(define T6 't6)

(define STU1 (make-student XUAN (list T1 T2)))
(define STU2 (make-student PAI (list T2 T3)))
(define STU3 (make-student SUD (list T3)))
(define STU4 (make-student HAN (list T4 T1)))
(define HANK (make-student 'hank (list T5 T4 T1)))
(define JAMES (make-student 'james (list T2 T1)))
(define ALI (make-student 'ali (list T3 T1 T5)))
(define THEO (make-student 'theo (list T6 T5)))
(define RICK (make-student 'rick (list T6 T5 T4 T3 T2 T1)))

(define STUS (list STU1 STU2 STU3 STU4))
(define STUDENTS (list STU1 STU2 STU3 STU4 HANK JAMES ALI THEO RICK))

(define CW1 (make-codewalk T1 '() 2))
(define CW2 (make-codewalk T2 '() 1))
(define CW3 (make-codewalk T2 '() 0))
(define CW4 (make-codewalk T3 '() 3))
(define CW5 (make-codewalk T4 '() 0))
(define CW6 (make-codewalk T5 '() 2))
(define CW7 (make-codewalk T6 '() 2))

(define CWS (list CW1 CW2 CW3 CW4 CW5))

(define CWS2 (list CW1 CW2 CW3 CW4 CW5 CW6 CW7))

; Function begins:

; check-for-avail/unavail Students CodeWalks Boolean -> Maybe<CodeWalks>
; Schedule codewalk according to the given student's time and checking if it's
; available or unavailable time
; Strategy: data decomposition on cws : CodeWalks
(begin-for-test
  (check-equal? (check-for-avail/unavail STUDENTS CWS2 true) 
                (list
                 (make-codewalk 't6 (list 'rick 'theo) 2)
                 (make-codewalk 't3 (list 'ali 'pai 'sud) 3)
                 (make-codewalk 't5 (list 'hank) 2)
                 (make-codewalk 't2 (list 'james) 1)
                 (make-codewalk 't1 (list 'han 'xuan) 2)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't4 empty 0))
                "Test failed. Function should return a codewalk schedule
according to student's available time")
  (check-equal? (check-for-avail/unavail STUS CWS false)
                (list
                 (make-codewalk 't2 (list 'sud) 1)
                 (make-codewalk 't3 (list 'han 'xuan) 3)
                 (make-codewalk 't1 (list 'pai) 2)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't4 empty 0))
                "Test failed. Function should return a codewalk schedule
according to student's available time"))
(define (check-for-avail/unavail students cws is-avail?)
  (if is-avail?
      (schedule students cws)
      (local ((define cw-times (map (lambda (cw) (codewalk-time cw)) cws))
              (define uniq-cw-times (set->list (list->set cw-times))))
        (schedule (student/unavail->avail students uniq-cw-times) cws)))) 

; schedule : StudentAvails CodeWalks -> Maybe<CodeWalks>
; schedule codewalks according to the given student's available time.
; Strategy: Function composition
(begin-for-test
  (check-equal? (schedule STUS CWS)
                (list
                 (make-codewalk 't1 (list 'han 'xuan) 2)
                 (make-codewalk 't2 (list 'pai) 1)
                 (make-codewalk 't3 (list 'sud) 3)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't4 empty 0))
                "schedual succeed.")
  (check-equal? (schedule STUDENTS CWS2)
                (list
                 (make-codewalk 't6 (list 'rick 'theo) 2)
                 (make-codewalk 't3 (list 'ali 'pai 'sud) 3)
                 (make-codewalk 't5 (list 'hank) 2)
                 (make-codewalk 't2 (list 'james) 1)
                 (make-codewalk 't1 (list 'han 'xuan) 2)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't4 empty 0))
                "schedual succeed.")
  (check-equal? (schedule (list STU3) (list CW5))
                #false
                "Test failed. Function should return false"))
(define (schedule students0 cws0)
  (local(;sched-cwlst: StudentAvails ListOf<CodeWalks> -> Maybe<CodeWalks>
         ;Assign a list of students stus to a lst of possible codewalks cws
         ;Strategy: Genertive recursion
         ;Halting Measure: the size of cws always decrease
         (define (sched-cwlst stus cws)
           (cond 
             [(empty? cws) #false]
             [else (local ((define candidate (schedule/a stus (first cws))))
                     (if (boolean? candidate)
                         (sched-cwlst stus (rest cws))
                         candidate))]))
         ;schedule/a: StudentAvails CodeWalks -> Maybe<CodeWalks>
         ;schedule codewalks according to the given student's available time.
         ;WHERE: cws-so-far is the codewalks scheduled so far for stuents0, and
         ;students represents the elements to be scheduled in student0
         ;Strategy: Generative recursion
         ;Halting Measure: the size of students always decrease in the  
         ;subproblems.
         (define (schedule/a students cws-so-far)
           (cond
             [(empty? students) cws-so-far]
             [else (local((define possible-cws (branches (first students)
                                                         cws-so-far)))
                     (sched-cwlst (rest students)
                                  possible-cws))])))
    (schedule/a (sort-by-prefs students0) cws0)))

; branches: StudentAvail CodeWalks -> ListOf<CodeWalks>
; Tries to assign every prefered time of the Student stu to the given CodeWalks 
; cws to generate a list of possible CodeWalks
; Strategy: Data decomposition on stu : StudentAvail
; Example:
(begin-for-test
  (check-equal? (branches STU1 CWS)
                (list
                 (list
                  (make-codewalk 't1 (list 'xuan) 2)
                  (make-codewalk 't2 empty 1)
                  (make-codewalk 't2 empty 0)
                  (make-codewalk 't3 empty 3)
                  (make-codewalk 't4 empty 0))
                 (list
                  (make-codewalk 't2 (list 'xuan) 1)
                  (make-codewalk 't1 empty 2)
                  (make-codewalk 't2 empty 0)
                  (make-codewalk 't3 empty 3)
                  (make-codewalk 't4 empty 0)))
                "two branches"))
(define (branches stu cws)
  (local ((define prefs (student-prefs stu))
          (define id (student-id stu)))
    (foldr (lambda (t l)  
             (local((define cws-after-assign (add-id/time-to-cw id t cws)))
               (if (false? cws-after-assign) 
                   l
                   (append (list cws-after-assign) l))))
           '()
           prefs)))

; add-id/time-to-cw : StudentId Time CodeWalks -> Maybe<CodeWalks>
; Adds one student's id and time to the given CodeWalks cws, if possible.
; Strategy: Data decomposition of cws : CodeWalks
(begin-for-test
  (check-equal? (add-id/time-to-cw XUAN T2 CWS)
                (list
                 (make-codewalk 't2 (list 'xuan) 1)
                 (make-codewalk 't1 empty 2)
                 (make-codewalk 't2 empty 0)
                 (make-codewalk 't3 empty 3)
                 (make-codewalk 't4 empty 0))
                "add Xuan to CWS")
  (check-equal? (add-id/time-to-cw XUAN T4 CWS)
                #false
                "no available cw slot"))
(define (add-id/time-to-cw id t cws)
  (local((define cw-avail (filter 
                           (lambda 
                               (cw) 
                             (and (symbol=? t (codewalk-time cw))
                                  (> (codewalk-max cw) 
                                     (length (codewalk-students cw)))
                                  ))
                           cws)))
    (if (empty? cw-avail)
        #false
        (cons (add-id-to-cw id (first cw-avail))
              (remove (first cw-avail) cws)))))

; add-id-to-cw : StudentId CodeWalk -> CodeWalk
; Returns a new CodeWalk from the given CodeWalk cwafter adding one student id 
; Strategy: data decomposition of cw : CodeWalk
(begin-for-test
  (check-equal? (add-id-to-cw XUAN CW1)
                (make-codewalk 't1 (list 'xuan) 2)
                "add Xuan to Cw1"))
(define (add-id-to-cw id cw)
  (make-codewalk (codewalk-time cw) 
                 (cons id (codewalk-students cw))
                 (codewalk-max cw)))

; sort-by-prefs : StudentAvails -> StudentAvails
; Returns a sorted list of students according to the time preferences.
; Sort accendingly by the number of prefs they have
; Strategy: Data decomposition on students: Students
; Example
(begin-for-test
  (check-equal? (sort-by-prefs STUS)
                (list STU3 STU1 STU2 STU4)
                "sort accendingly student by the number of prefs they have"))
(define (sort-by-prefs students)
  (sort students
        (lambda (s1 s2) (< (length (student-prefs s1))
                           (length (student-prefs s2))))))


; check-for-avail/unavail-ok? Students CodeWalks Boolean -> Boolean
; Returns true if cws is a valid schedule according to the given
; student available or unavailable time.
; Strategy: Data decomposition on cws : CodeWalks
(begin-for-test
  (check-true (check-for-avail/unavail-ok? STUS 
                                           (list
                                            (make-codewalk 't1 (list 'han 
                                                                     'xuan) 2)
                                            (make-codewalk 't2 (list 'pai) 1)
                                            (make-codewalk 't3 (list 'sud) 3)
                                            (make-codewalk 't2 empty 0)
                                            (make-codewalk 't4 empty 0)) true))
  (check-true (check-for-avail/unavail-ok? STUS 
                                           (list
                                            (make-codewalk 't1 
                                                           (list 'sud 'pai) 2)
                                            (make-codewalk 't3 
                                                           (list 'han 'xuan) 3)
                                            (make-codewalk 't2 empty 1)
                                            (make-codewalk 't2 empty 0)
                                            (make-codewalk 't4 empty 0))
                                           false)))
(define (check-for-avail/unavail-ok? students cws is-avail?)
  (if is-avail?
      (schedule-ok? students cws)
      (local ((define cw-times (map (lambda (cw) (codewalk-time cw)) cws))
              (define uniq-cw-times (set->list (list->set cw-times))))
        (schedule-ok? (student/unavail->avail students uniq-cw-times) 
                      cws))))


; student/unavail->avail: StudentUnavails ListOf<Time> -> StudentAvails
; Converts StudentUnavails stus to StudentAvails according to given listofTime
; Strategy: data decomposition of stus : StudentUnavails
(begin-for-test
  (check-equal? (student/unavail->avail (list (make-student 'xuan '(2 4))
                                              (make-student 'sud '(4))) 
                                        '(1 2 3 4))
                (list
                 (make-student 'xuan (list 1 3))
                 (make-student 'sud (list 1 2 3)))
                ""))
(define (student/unavail->avail stus uniq-cw-times)
  (cond 
    [(empty? stus) '()]
    [else (cons (replace-stu-prefs (first stus) uniq-cw-times)
                (student/unavail->avail (rest stus) uniq-cw-times))]))

; replace-stu-prefs: StudentUnavail ListOf<Time> -> StudentAvail
; Returns a StudentAvail by changing it's unavailable time to available time 
; Strategy: data decomposition on stu: StudentUnavail
(begin-for-test
  (check-equal? (replace-stu-prefs (make-student 'xuan '(2 4)) '(1 2 3 4))
                (make-student 'xuan (list 1 3))
                ""))
(define (replace-stu-prefs stu uniq-cw-times)
  (local ((define avail-times (delete-unavail-times (student-prefs stu)
                                                    uniq-cw-times)))
    (make-student (student-id stu)
                  avail-times)))

; delete-unavail-times: ListOf<Time> ListOf<Time> -> ListOf<Time>
; Returns a list of time that does not contain any elements in unavail-times0
; Strategy: function composition
(begin-for-test
  (check-equal? (delete-unavail-times '(1) '(1 2 3 4))
                (list 2 3 4)))
(define (delete-unavail-times unavail-times0 uniq-cw-times0)
  (local (;delete-unavail-times/a: ListOf<Time> ListOf<Time> -> ListOf<Time>
          ;Returns a list of time that does not contain any elements in 
          ;unavail-times0
          ;WHERE: avail-times-so-far is the elements that reamains in 
          ;uniq-cw-times0, unavail-times is the elements in unavail-times0
          ;that need to be deleted from avail-times-so-far.
          ;Strategy: data decomposition on unavail-times : ListOf<Time>
          (define (delete-unavail-times/a unavail-times avail-times-so-far)
            (cond
              [(empty? unavail-times) avail-times-so-far]
              [else (delete-unavail-times/a (rest unavail-times)
                                            (remove (first unavail-times)
                                                    avail-times-so-far))])))
    (delete-unavail-times/a unavail-times0 uniq-cw-times0)))

; schedule-ok? : StudentAvails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; Strategy: Function composition
; Example:
(begin-for-test
  (check-true (schedule-ok? STUS (list
                                  (make-codewalk 't1 (list 'han 'xuan) 2)
                                  (make-codewalk 't2 (list 'pai) 1)
                                  (make-codewalk 't3 (list 'sud) 3)
                                  (make-codewalk 't2 empty 0)
                                  (make-codewalk 't4 empty 0))) 
              "schedule ok")
  (check-false (schedule-ok? STUS (list
                                   (make-codewalk 't1 (list 'han 'xuan) 2)
                                   (make-codewalk 't2 (list 'pai 'sud) 2)
                                   (make-codewalk 't3 (list 'sud) 3)
                                   (make-codewalk 't2 empty 0)
                                   (make-codewalk 't4 empty 0))) 
               "invalid schedule")
  (check-false (schedule-ok? STUS (list
                                   (make-codewalk 't1 (list 'han 'xuan) 2)
                                   (make-codewalk 't2 (list 'pai 'sud) 2)
                                   (make-codewalk 't3 empty 3)
                                   (make-codewalk 't2 empty 0)
                                   (make-codewalk 't4 empty 0))) 
               "invalid schedule")
  (check-false (schedule-ok? '() (list
                                  (make-codewalk 't1 (list 'han 'xuan) 2)
                                  (make-codewalk 't2 (list 'pai 'sud) 2)
                                  (make-codewalk 't3 empty 3)
                                  (make-codewalk 't2 empty 0)
                                  (make-codewalk 't4 empty 0))) 
               "invalid schedule"))
(define (schedule-ok? students cws)
  (cond
    [(or (empty? students) (empty? cws)) false]
    [else (and (is-capacity-ok? cws) 
               (match-students? (get-ids students) (get-codewalk-stu cws))
               (is-acceptable-time? students cws))]))

; is-capacity-ok? : CodeWalks -> Boolean
; Returns true if each codewalk is not over capacity
; Strategy: data decomposition on cws : CodeWalks
; Example: 
(begin-for-test 
  (check-true (is-capacity-ok? (list
                                (make-codewalk 't1 (list 'han 'xuan) 2))) 
              "ok")
  (check-false (is-capacity-ok? (list
                                 (make-codewalk 't1 
                                                (list 'han 'xuan 'Sam) 2))) 
               "capacity excceed"))
(define (is-capacity-ok? cws)
  (andmap (lambda (x) (<= (length (codewalk-students x)) 
                          (codewalk-max x))) 
          cws))

; get-codewalk-stu : CodeWalks -> ListOf<StudentId>
; Returns a list of all the students who have been assigned a codewalk
; Strategy: Data decomposition on cws : CodeWalks
; Example:
(begin-for-test
  (check-equal? (get-codewalk-stu (list
                                   (make-codewalk 't6 (list 'rick 'theo) 2)
                                   (make-codewalk 't3 (list 'ali 'pai 'sud) 3)
                                   (make-codewalk 't5 (list 'hank) 2)
                                   (make-codewalk 't2 (list 'james) 1)
                                   (make-codewalk 't1 (list 'han 'xuan) 2)
                                   (make-codewalk 't2 empty 0)
                                   (make-codewalk 't4 empty 0)))
                (list 'rick 'theo 'ali 'pai 'sud 'hank 'james 'han 'xuan)
                "ids retured."))
(define (get-codewalk-stu cws)
  (cond
    [(empty? cws) '()]
    [else (append (codewalk-students (first cws)) 
                  (get-codewalk-stu (rest cws)))]))

; get-ids : StudentAvails -> ListOf<StudentId>
; Returns a list of ids of all the students
; Strategy: Data decomposition on stu : StudentAvails
; Example:
(begin-for-test
  (check-equal? (get-ids STUDENTS)
                (list 'xuan 'pai 'sud 'han 'hank 'james 'ali 'theo 'rick)
                "ids retured."))
(define (get-ids stus)
  (cond
    [(empty? stus) '()]
    [else (cons (student-id (first stus)) 
                (get-ids (rest stus)))]))

; match-students? : ListOf<StudentId> ListOf<StudentId> -> Boolean
; Returns true if every student is placed in only one codewalk
; Strategy: Function composition 
; Example:
(begin-for-test
  (check-true (match-students? (list 'sud 'xuan 'han 'pai) 
                               (list 'han 'xuan 'sud 'pai))
              "valid match")
  (check-true (match-students? (list 'sud 'xuan 'han 'pai) 
                               (list 'sud 'xuan 'han 'pai 'theo))
              "valid match"))
(define (match-students? students los)
  (local(;get-matching-student : StudentId -> Boolean
         ;Returns true if a student id appears only once in los
         ;Strategy: Function composition
         (define (get-matching-student student)
           (= (length 
               (filter (lambda (x) (symbol=? x student)) 
                       los))
              1)))
    (andmap get-matching-student students)))

; is-acceptable-time? : StudentAvails CodeWalks -> Boolean
; Returns true if all the students have codewalk on their preferred time
; Strategy: Function composition
; Example:
(begin-for-test
  (check-true (is-acceptable-time? STUDENTS 
                                   (list
                                    (make-codewalk 't6 (list 'rick 'theo) 2)
                                    (make-codewalk 't3 (list 'ali 'pai 'sud) 3)
                                    (make-codewalk 't5 (list 'hank) 2)
                                    (make-codewalk 't2 (list 'james) 1)
                                    (make-codewalk 't1 (list 'han 'xuan) 2)
                                    (make-codewalk 't2 empty 0)
                                    (make-codewalk 't4 empty 0)))
              "valid time"))
(define (is-acceptable-time? students cws)
  (local(;check-preference : Student -> Boolean
         ;Returns true if student gets a codewalk according to preference
         ;Strategy: data decomposition on stu : Student
         (define (check-preference stu)
           (not (empty? 
                 (filter (lambda (cw) 
                           (and (member? (student-id stu) 
                                         (codewalk-students cw))
                                (member? (codewalk-time cw) 
                                         (student-prefs stu)))) 
                         cws)))))
    (andmap check-preference students)))