;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eval_) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 30)

(provide eval)
(provide lambda?)
(provide errstr?)
(provide subst)
(provide expr->expr/no-var)
(provide expr=?)

; A UniqueListOf<X> is a ListOf<X>
; WHERE: none of the elements of the list are equal? to each other
(define ulst '(1 2 3))
;Template: 
;STRATEGY : Data decomposition on l : UniqueListOf<X>
(define (unique-list-fn l)
  (cond
    [(empty? l) ...]
    [else
     (...(first l) ... (unique-list-fn (rest l)))]))
 
; A 2ListOf<X> is a (cons X (cons X ListOf<X>))
; Represents a list of 2 or more elements.
(define 2lox '(1 2))
; Template:
; 2ListOf<X> -> ???
; STRATEGY: data decomposition on 2lox : 2ListOf<X>
; WHERE: X is Expr or ExprNoVar
(define (2lox-fn 2lox)
  (... (x-fn (first 2lox)) ...
   ... (x-fn (second 2lox))... 
   ...(list-fn (rest (rest 2lox)))...))

; A Program is a:
; - empty
; - (cons Expr Program)
; - (cons Def Program)
; Represents a PDPLang program consisting of function defs and expressions.
; WHERE: no two Defs have the same name 
; Template:
; program-fn :Program -> ???
; STRATEGY: data decomposition on p : Program
(define (program-fn p)
  (cond 
    [(empty? p) ...]
    [(not (def? (first p))) 
     (...(expr-fn (first p))... (program-fn (rest p))...)]
    [else (...(def-fn (first p))... (program-fn (rest p))...)]))

; An Expr is one of:
; - Number
; - Boolean
; - Var
; - ErrString
; - Lambda  
; - (make-arith ArithOp 2ListOf<Expr>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<Expr>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<Expr>)     ; a comparison expression
; - (make-if-exp Expr Expr Expr) ; an if conditional
; - (make-call Expr ListOf<Expr>) ; a function call
; Represents a PDPLang expression.

; Template:
; expr-fn :Expr -> ???
; STRATEGY: data decomposition on expr : Expr
(define (expr-fn expr)
  (cond
    [(number? expr) ...]
    [(boolean? expr) ...]
    [(symbol? expr) ...]
    [(string? expr) ...]
    [(lam? expr) (...(lambda-fn expr)...)]
    [(arith? expr) (...(arithop-fn (arith-op expr))... 
                       ...(2lox-fn (arith-args expr))...)]
    [(bool? expr) (...(boolop-fn (bool-op expr))... 
                       ...(2lox-fn (bool-args expr))...)]
    [(cmp? expr) (...(cmpop-fn (cmp-op expr))... 
                       ...(2lox-fn (cmp-args expr))...)]
    [(if-exp? expr) (...(expr-fn (if-exp-test expr))...
                        ...(expr-fn (if-exp-branch1 expr))...
                        ...(expr-fn (if-exp-branch2 expr))...)]
    [else (...(expr-fn (call-fn expr))...
           ...(list-fn (call-args expr)...))]))

(define-struct arith (op args))
; An Arith is a (make-arith ArithOp 2ListOf<Expr>)
; Represents an arithmatic operation which contains an arithmatic 
; operator op and two or more operands args
(define a1 (make-arith '+ '(1 2)))
; Template:
; arith-fn :Arith -> ???
; STRATEGY : Data decomposition on a : Arith
(define (arith-fn a)
  (... (arithop-fn (arith-op a)) ... (2lox-fn (arith-args a))))

(define-struct bool (op args))
; A Bool is a (make-bool BoolOp 2ListOf<Expr>)
; Represents a boolean operation which contains a boolean operator op
; and two or more operands args
(define aa1 (make-bool 'and '(#true #false)))
; Template:
; bool-fn :Bool -> ???
; STRATEGY : Data decomposition on b : Bool
(define (bool-fn b)
  (... (boolop-fn (bool-op b)) ... (2lox-fn (bool-args b))))

(define-struct cmp (op args))
; A Cmp is a (make-cmp CmpOp 2ListOf<Expr>)
; Represents a comparison operation which contains a comparison operator op
; and two or more operands args
(define b1 (make-cmp '> '(1 2)))
; Template:
; cmp-fn : Cmp -> ???
;STRATEGY : Data decomposition on c : Cmp
(define (cmp-fn a)
  (... (cmpop-fn (cmp-op c)) ... (2lox-fn (cmp-args c))))

(define-struct if-exp (test branch1 branch2))
; An If-exp is a (make-if-exp Expr Expr Expr)
; Represents an if-expression which contains a condition and 
; two branch statements. 
; The result of the If-expression is the evaluation of the first branch if
; the test condition evaluates to true, otherwise the result is the evaluation 
; of the second branch statement.
(define ie1 (make-if-exp #true 2 3))
; Template
; if-exp-fn :If-exp -> ???
; STRATEGY : Data decomposition on i : If-exp
(define (if-exp-fn i)
  (... (expr-fn (if-exp-test i)) ... (expr-fn (if-exp-branch1 i))  
       ... (expr-fn (if-exp-branch2 i))))

(define-struct call (fn args))
; A Call is a (make-call Expr ListOf<Expr>)
; Represents a function call with a function name fn and parameters args
; Template :
(define c1 (make-call 'x '(1 2)))
; call-func :Call -> ???
; STRATEGY : Data decomposition on c : Call
(define (call-func c)
  (... (call-fn c) ... (call-args c)))

; A Var is a Symbol, representing PDPLang variable.
(define v1 'x)

; An ErrString is a String, representing a PDPLang error message.
(define err1 "err: test")

(define-struct lam (params body))
; A Lambda is a (make-lam UniqueListOf<Param> Expr)
; Represents a lambda expression in PDPLang
(define lam1 (make-lam '(x y) 'y))
; Template:
; lambda-fn :Lam -> ???
;STRATEGY : Data decomposition on l : Lambda
(define (lambda-fn l)
  ( .... (unique-list-fn (lambda-params l) ... (expr-fn (lambda-body)))))

; A Param is a Var, representing a function parameter.
(define pp1 'v)

; An ArithOp is one of:
; - '+
; - '-
; - '*
; - '/
; Represents an arithmetic operation in PDPLang
(define ADD '+)
(define SUB '-)
(define MUL '*)
(define DIV '/)
; Template:
; arithop-fn :ArithOp -> ???
; STRATEGY: Data decomposition on a: ArithOp
(define (arithop-fn a)
  (cond
    [(symbol=? ADD a) ...]
    [(symbol=? SUB a) ...]
    [(symbol=? MUL a) ...]
    [(symbol=? DIV a) ...]))
      
; A BoolOp is one of:
; - 'and
; - 'or
; Represents a boolean operation in PDPLang
(define AND 'and)
(define OR 'or)
; Template :
; boolop-fn: BoolOp -> ???
;STRATEGY: Data decomposition on b : BoolOp
(define (boolop-fn b)
  (cond
    [(symbol=? AND b) ...]
    [(symbol=? OR b) ...]))

; A CmpOp is one of:
; - '=
; - '<
; - '>
; Represents a comparison operation in PDPLang
(define EQUAL '=)
(define LT '<)
(define GT '>)
; TEMPLATE : 
; cmpop-fn :CmpOp -> ???
; STRATEGY : Data decompositio on c : CmpOp
(define (cmpop-fn c)
  (cond
    [(symbol=? EQUAL c) ...]
    [(symbol=? LT c) ...]
    [(symbol=? GT c) ...]))

(define-struct def (name params body))
; A Def is a (make-def FnName UniqueListOf<Param> Expr)
; Represents the definition of a function with the specified parameters.
(define deff1 (make-def 'fx '(x) 'x))
; Template:
; def-fn : Def -> ???
; STRATEGY: Data decomposition on d : Def
(define (def-fn d)
  (... (def-name d) ... (unique-list-fn (def-params d)) ... 
       (expr-fn (def-body d))))
 
; A FnName is a Var, representing a function name.
(define fname1 'gx)

; A Result is a:
; - Number
; - Boolean
; - ErrString
; - Lambda
; Represents the data after it is evaluated ???
(define re1 0)
; Template: 
; result-fn :Result -> ???
; STRATEGY: Data decomposition on r : Result
(define (result-fn r)
  (cond
    [(number? r) ...]
    [(boolean? r) ...]
    [(string? r) ...]
    [(lam? r) (...(lambda-fn r)...)]))

; An ExprNoVar is one of:
; - Number
; - Boolean
; - StaticDist
; - ErrString
; - LamNoVar
; - (make-arith ArithOp 2ListOf<ExprNoVar>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<ExprNoVar>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<ExprNoVar>)     ; a comparison expression
; - (make-if-exp ExprNoVar ExprNoVar ExprNoVar) ; an if conditional
; - (make-call ExprNoVar ListOf<ExprNoVar>) ; a function call
; Represents an Expr without explicit variables.
(define ex/no/var1 12)
; Template:
; expr-no-var-fn :ExprNoVar -> ???
; STRATEGY: data decomposition on expr-no-var : ExprNoVar
(define (expr-no-var-fn expr-no-var)
  (cond
    [(number? expr-no-var) ...]
    [(boolean? expr-no-var) ...]
    [(list? expr-no-var) ...]
    [(string? expr-no-var) ...]
    [(lam/no-var? expr-no-var) ...]
    [(arith? expr-no-var) (...(arithop-fn (arith-op expr-no-var))... 
                       ...(2lox-fn (arith-args expr-no-var))...)]
    [(bool? expr-no-var) (...(boolop-fn (bool-op expr-no-var))... 
                       ...(2lox-fn (bool-args expr-no-var))...)]
    [(cmp? expr-no-var) (...(cmpop-fn (cmp-op expr-no-var))... 
                       ...(2lox-fn (cmp-args expr-no-var))...)]
    [(if-exp? expr-no-var) (...(expr-no-var-fn (if-exp-test expr-no-var))...
                        ...(expr-no-var-fn (if-exp-branch1 expr-no-var))...
                        ...(expr-no-var-fn (if-exp-bramch2 expr-no-var))...)]
    [else (...(expr-no-var-fn (call-fn expr-no-var))...
           ...(list-fn (call-args expr-no-var)...))]))
 
; A StaticDist is a (list Depth Index)
; Represents a variable reference
; where depth is number of additional lambdas between this var ref and the
; lambda for which this variable is a parameter,
; and index is the (0-based) position of this variable in that lambda's
; parameter list.
(define sd1 '(0 0))
; Template:
; static-dist-fn :StaticDist -> ???
; STRATEGY: Data decomposition on s : StaticDist
(define (static-dist-fn s)
  (...(first s) ... (second s)))

; A Depth is a Natural
(define d1 1)

; An Index is a Natural
(define i1 0)

; A LamNoVar is a (make-lam/no-var ExprNoVar)
(define-struct lam/no-var (body))
; Represents a lambda expression in PDPLang without explicit variables
(define lnv1 '(1 0))
; Template
; lam/no-var -> ???
; STRATEGY : Data decomposition on lnv : lam/no-var
(define (lam/no-var-fn lnv)
  (...(expr-no-var-fn (lam/no-var-body lnv))...))

;Examples
(define A1 (make-arith '/ (list 15 5)))
(define A2 (make-arith '* (list A1 9)))
(define A3 (make-arith '+ (list 998 true)))
(define C1 (make-cmp 'or (list #true #false #true #false #true #false)))
(define C2 (make-cmp 'and (list #true #false #true #false #true #false)))

(define IF-EXPR1
  (make-if-exp (make-cmp '= (list A1 10))
               (make-arith '+ (list A1 A2 A3))
               (make-bool 'and (list C1 C2 #false))))

(define IF-EXPR2
  (make-if-exp (make-cmp '> (list A1 10))
               (make-arith '/ (list A1 A2 A3))
               (make-bool 'and (list C1 C2 C2 C1))))

(define IF-EXPR3
  (make-if-exp (make-arith '/ (list A1 A2 A3))
               (make-cmp '> (list A1 10))
               (make-bool 'and (list C1 C2 C2 C1))))

(define LAM (make-lam '(x y) (make-arith '+ '(x y))))

(define F (make-def 'f (list 'x 'y 'z) 
                    (make-if-exp (make-cmp '> (list 'x 'y))
                                 (make-call 'g (list 'x 'y 'z))
                                 (make-arith '* (list 'x 'y 'z)))))

(define G (make-def 'g 
                     (list 'x 'y 'z)
                     (make-arith '+ (list 'x 'y 'z))))

(define CALL1 (make-call 'f (list 8 4 2)))

(define P 
  (list G IF-EXPR1 IF-EXPR2 'f 'g 10 "err: test-err" F #false LAM CALL1))

(define D (make-arith '/ (list 0 2 0 1)))

(define I (list F G D))

(define P1 (list F CALL1))

(define DEF (list
 (make-def
  'g
  (list 'x 'y 'z)
  (make-arith '+ (list 'x 'y 'z)))
 (make-def
  'f
  (list 'x 'y 'z)
  (make-if-exp
   (make-cmp '> (list 'x 'y))
   (make-call 'g (list 'x 'y 'z))
   (make-arith '* (list 'x 'y 'z))))))

(define P2 (list CALL1 G))

(define P3 (list CALL1 F))
 
;Functions begin 

; eval : Program -> ListOf<Result>
; Evaluates a PDPLang program to a list of Results.
; Specifically, evaluates the Exprs in p, in the context of the given Defs.
; WHERE: A function may be called before it is defined.
; WHERE: The produced results are in the same relative order as their 
; originating Exprs in p.
; STRATEGY : Function composition
(begin-for-test 
  (check-equal? (eval P)
                (list
                 false
                 false
                 (make-lam
                  (list 'x 'y 'z)
                  (make-if-exp
                   (make-cmp '> (list 'x 'y))
                   (make-call 'g (list 'x 'y 'z))
                   (make-arith '* (list 'x 'y 'z))))
                 (make-lam
                  (list 'x 'y 'z)
                  (make-arith '+ (list 'x 'y 'z)))
                 10
                 "err: test-err"
                 false
                 (make-lam (list 'x 'y) (make-arith '+ (list 'x 'y)))
                 14)
                "Evaluated P"))
(define (eval p)
  (local ((define defs (filter-defs p)))
    (eval-with-defs p defs)))

; eval-with-defs : Program ListOf<Def> -> ListOf<Result>
; Evaluates the Program, p in context of the defs
; STRATEGY : Data decomposition on p : Program
(begin-for-test 
  (check-equal? (eval-with-defs P DEF)
                (list
                 false
                 false
                 (make-lam
                  (list 'x 'y 'z)
                  (make-if-exp
                   (make-cmp '> (list 'x 'y))
                   (make-call 'g (list 'x 'y 'z))
                   (make-arith '* (list 'x 'y 'z))))
                 (make-lam
                  (list 'x 'y 'z)
                  (make-arith '+ (list 'x 'y 'z)))
                 10
                 "err: test-err"
                 false
                 (make-lam (list 'x 'y) (make-arith '+ (list 'x 'y)))
                 14)
                "Evaluated P")
  (check-equal? (eval-with-defs I DEF)
                (list "err: divide-by-zero")
                "Not possible to divide by zero"))
(define (eval-with-defs p defs)
  (cond 
       [(empty? p) '()]
       [(not (def? (first p))) 
        (cons (expr->result (first p) defs) (eval-with-defs (rest p) defs))]
       [else (eval-with-defs (rest p) defs)]))

; filter-defs : Program -> ListOf<Def>
; Seperates out the defs from Program
; STRATEGY : Data decomposition on p : Program
(begin-for-test
  (check-equal? (filter-defs P)
                (list
                 (make-def
                  'g
                  (list 'x 'y 'z)
                  (make-arith '+ (list 'x 'y 'z)))
                 (make-def
                  'f
                  (list 'x 'y 'z)
                  (make-if-exp
                   (make-cmp '> (list 'x 'y))
                   (make-call 'g (list 'x 'y 'z))
                   (make-arith '* (list 'x 'y 'z)))))))
(define (filter-defs p)
  (cond
    [(empty? p) '()]
    [(not (def? (first p))) (filter-defs (rest p))]
    [else (cons (first p) (filter-defs (rest p)))]))

; expr->result : Expr ListOf<Def> -> Result
; Evaluates the Expr in context of the defs 
; STRATEGY : Data decomposition on e : Expr
(begin-for-test 
  (check-equal? (expr->result (make-arith '+ (list 10 20 30))
                             (make-def 'f (list 'x 'y)
                                       (make-bool 'and (list 'x 'y))))
               60
               "Added the numbers to get 60"))
(define (expr->result expr defs)
  (cond
    [(or (number? expr) (boolean? expr) (string? expr) (lam? expr)) expr]
    [(symbol? expr) (eval-symbol expr defs)]
    [(arith? expr) 
     (eval-operation (arith-op expr) 
                     (loex->lore (arith-args expr) defs))]
    [(bool? expr)
     (eval-operation (bool-op expr) 
                     (loex->lore (bool-args expr) defs))]
    [(cmp? expr)
     (eval-operation (cmp-op expr) 
                      (loex->lore (cmp-args expr) defs))]
    [(if-exp? expr) (eval-if-expr expr defs)]
    [else (eval-call expr defs)]))

; eval-call: Expr List<Def> -> Result
; Evaluates a Call Expr(function call)
; STRATEGY : Data decomposition on ca : Call
(begin-for-test
  (check-equal? (eval-call (make-call 'j (list 1 2 3)) DEF)
                "err: subfn is NOT lambda in call"
                "Prints error"))
(define (eval-call ca defs)
  (local ((define subfn (expr->result (call-fn ca) defs))
          (define args (loex->lore (call-args ca) defs))
          ;eval0 : Lambda -> Result
          ;Returns a result for the given lambda lam
          ;Strategy: data decomposition on lam : Lambda
          (define (eval0 lam)
            (expr->result (mul-subst args
                                     (lam-params lam)
                                     (lam-body lam)) 
                      defs)))
    (if (lambda? subfn)
        (eval0 subfn)
        "err: subfn is NOT lambda in call")))

; mul-subst : ListOf<Result> ListOf<Expr> Expr -> Expr
; Replaces every reference to every item to args0 in expr0 with 
; the corresponding item in results0
; STRATEGY : function composition
(begin-for-test
  (check-equal? (mul-subst (list 10 20) (list 'x 'y 'z) (make-arith '+ '(x y)))
                "err: incompatable arguments numbers"
                "Prints error"))
(define (mul-subst results0 args0 expr0)
  (local ((define compatable? (= (length results0) (length args0)))
          ;ListOf<Result> ListOf<Expr> Expr -> Expr
          ;Replaces every reference to every item to args in expr with 
          ;the corresponding item in results
          ;WHERE: expr-replace-so-far is the expr that has been replaced so far,
          ;the results is the elements in results0 to replace and args is the 
          ;elements in args0 to be replaced by results correspondingly.
          ;Strategy: Data decomposition on results : ListOf<Result> and 
          ;args : ListOf<Expr>
          (define (mul-subst/a results args expr-replace-so-far)
            (cond
              [(empty? results) expr-replace-so-far]
              [else (mul-subst/a (rest results) 
                                 (rest args)
                                 (subst (first results) 
                                        (first args) 
                                        expr-replace-so-far))])))
    (if compatable?
        (mul-subst/a results0 args0 expr0)
        "err: incompatable arguments numbers")))

; eval-if-expr: If-Expr -> Result 
; Evaluates the If-expr
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (eval-if-expr IF-EXPR3 G)
                "err: test result invaild"
                "Error found"))
(define (eval-if-expr ie defs)
  (local ((define test-result (expr->result (if-exp-test ie) defs)))
    (if (boolean? test-result)
        (if test-result
            (expr->result (if-exp-branch1 ie) defs)
            (expr->result (if-exp-branch2 ie) defs))
        "err: test result invaild")))

; subst : Result Var Expr -> Expr
; Replaces references to x in expr with r.
; Does not replace x with r if x occurs in the body of a lambda
; that shadows x.
; WHERE: r has no unbound variables
; Strategy: data decompostion on expr : Expr
(begin-for-test
  (check-equal? (subst 4 'x 'x) 4 "x matches")
  (check-equal? (subst 4 'y 'x) 'x "y doesnt match")
  (check-equal?
   (subst 4 'x (make-arith '+ '(x 5)))
   (make-arith '+ '(4 5))
   "subst in arith")
  (check-equal?
   (subst 4 'x (make-lam '(y) (make-arith '+ '(x y))))
   (make-lam '(y) (make-arith '+ '(4 y)))
   "subst in lambda")
  (check-equal?
   (subst 4 'x (make-lam '(y) (make-bool 'and '(x y))))
   (make-lam (list 'y) (make-bool 'and (list 4 'y)))
   "subst in lambda")
  (check-equal?
   (subst 4 'x (make-lam '(x) (make-arith '+ '(x 5))))
   (make-lam '(x) (make-arith '+ '(x 5)))
   "dont subst shadowed vars in lambdas"))
(define (subst r x expr) 
  (cond 
    [(or (number? expr) (boolean? expr) (string? expr)) expr]
    [(symbol? expr) (if (eq? expr x) r expr)]
    [(lam? expr) (if (member? x (lam-params expr))
                     expr
                     (make-lam (lam-params expr) (subst r x (lam-body expr))))] 
    [(arith? expr) (make-arith (arith-op expr)
                               (subst-in-lst r x (arith-args expr)))]
    [(bool? expr) (make-bool (bool-op expr)
                             (subst-in-lst r x (bool-args expr)))]
    [(cmp? expr) (make-cmp (cmp-op expr)
                           (subst-in-lst r x (cmp-args expr)))]
    [(if-exp? expr) (make-if-exp (subst r x (if-exp-test expr))
                                 (subst r x (if-exp-branch1 expr))
                                 (subst r x (if-exp-branch2 expr)))]
    [else (make-call (call-fn expr)
                     (subst-in-lst r x (call-args expr)))]))

; subst-in-lst : Result Var ListOf<Expr> -> ListOf<Expr> 
; Performs subst for a list of expressions (Exprs)
; STRATEGY : Function composition
(begin-for-test
  (check-equal?
   (subst-in-lst 4 'x '(x 5))
   (list 4 5)
   "Replaced x"))
(define (subst-in-lst r x args)
  (map (lambda (e) (subst r x e))
       args))

; eval-symbol: Var List<Def> -> Lambda
; Evaluates a symbol
; STRATEGY : function composition
(begin-for-test
  (check-equal? (eval-symbol 'x DEF)
                "err: undefined var"
                "Print error"))
(define (eval-symbol var defs)
  (local (;find-def : Var List<Def> -> Maybe<Def>
          ;Returns the function name whose function name equates to var, if 
          ;there is any.
          ;STRATEGY: data decomposition on defs: ListOf<Def>
          (define (find-def var defs)
            (cond
              [(empty? defs) #false]
              [else (if (eq? var (def-name (first defs)))
                        (first defs)
                        (find-def var (rest defs)))]))
          ;(= result true) if the Symbol var represents a function in defs
          (define result (find-def var defs))
          ; mk-lam : Def -> Lambda
          ; Returns a Lambda for the given Def fn
          ; Strategy: data decomposition on fn : Def
          (define (mk-lam fn)
            (make-lam (def-params fn) (def-body fn))))
    (if (boolean? result)
        "err: undefined var"
        (mk-lam result))))

; loex->lore : ListOf<Expr> -> ListOf<Result>
; Evaluates every expr in the list and produces its corresponding result
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (loex->lore '(1 2) '())
                '(1 2)
                "expressions 1, 2 will be evaluated to 1, 2"))
(define (loex->lore lox defs)
  (map (lambda (e) (expr->result e defs)) lox))

; eval-operation: [ArithOp or BoolOp or ComOp] 2ListOf<Result> -> Result
; Evaluates all arithmatic, boolean and comparison operations
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (eval-operation '+ (list 1 2 3))
                6
               "Adds numbers"))
(define (eval-operation op lst)
  (if (compatable? lst)
      (operate-on-lst op lst)
      "err: incompatable between operator and args"))

; operate-on-lst : [ArithOp or BoolOp or ComOp] 2ListOf<Result> -> Result
; Returns the result after evaluating the operation
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (operate-on-lst '- (list 40 2 5))
                33
               "Substracts numbers"))
(define (operate-on-lst op lst)
  (if (arithop? op)
      (arith-on-lst op lst)
      (if (cmpop? op)
          (com-on-lst op lst)
          (bool-on-lst op lst))))

; arithop? : Symbol -> Boolean
; Returns true if the operation is a ArithOp
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (arithop? '+)
                true
               "Returns true"))
(define (arithop? op)
  (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '/)))

; cmpop? : Symbol -> Boolean
; Returns true if the operation is a CmpOp
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (cmpop? '=)
                true
               "Returns true"))
(define (cmpop? op)
  (or (eq? op '>) (eq? op '=) (eq? op '<)))

; operate-bool-on-lst : BoolOp 2ListOf<Result> -> Result
; Evaluates all boolean operations to a Result
; STRATEGY : function composition
(begin-for-test
  (check-equal? (bool-on-lst 'and (list true false))
                false
               "Returns false"))
(define (bool-on-lst op lst)
  (arith/bool-operate (boolop->fn op) 0 (sub1 (length lst)) lst))

; compatable? :ListOf<Result> -> Boolean
; Returns true if the list is a list of numbers or list of boolean
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (compatable? (list true false))
                true
               "Returns true"))
(define (compatable? lst)
  (local (; one-type? : ListOf<Result> -> Boolean
          ; Returns true if the list is a list of numbers or list of boolean
          ; STRATEGY : Function composition
          (define (one-type? lst)
            (or (andmap (lambda (r) (number? r))
                        lst)   
                (andmap (lambda (r) (boolean? r))
                        lst))))
    (one-type? lst)))

; arithop->fn : ArithOp -> Any
; Returns the operator for it's corresponding symbol 
; STRATEGY : Data decomposition on aop : ArithOp
(begin-for-test 
  (check-equal? (arithop->fn '-)
                -
                "Returns -"))
(define (arithop->fn aop)
  (cond 
    [(eq? aop '+) +]
    [(eq? aop '-) -]
    [(eq? aop '*) *]
    [(eq? aop '/) /]))

; comop->fn : CmpOp -> Any
; Returns the operator for it's corresponding symbol 
; STRATEGY : Data decomposition on aop : ComOp
(begin-for-test 
  (check-equal? (comop->fn '<)
                <
                "Returns <"))
(define (comop->fn aop)
  (cond 
    [(eq? '> aop) >]
    [(eq? '= aop) =]
    [(eq? '< aop) <]))

; boolop->fn: BoolOp -> Any
; Returns the operator for it's corresponding symbol 
; STRATEGY : Data decomposition on aop : BoolOp
(begin-for-test
  (check-true ((boolop->fn 'and) #true #true)
              "true and true is true"))
(define (boolop->fn aop)
  (cond
    [(eq? aop 'and) (lambda (x y) (and x y))]
    [(eq? aop 'or) (lambda (x y) (or x y))]))

; arith-on-lst : ArithOp 2ListOf<Result> -> Result
; Evaluates arithmatic operations
; STRATEGY : function composition
(begin-for-test
  (check-equal? (arith-on-lst '+ (list 1 2 3))
                6
                "Adds numbers"))
(define (arith-on-lst aop alst)
  (local ((define divide-by-zero? (and (eq? / (arithop->fn aop)) 
                                       (ormap (lambda (x) (zero? x))
                                                         (rest alst)))))
    (if divide-by-zero?
        "err: divide-by-zero"
        (arith/bool-operate (arithop->fn aop) 0 (sub1 (length alst)) alst))))

; com-on-lst : 2ListOf<Result> -> Result
; Evaluates the comparison operations
; STRATGY : function composition
(begin-for-test
  (check-equal? (com-on-lst '< (list 20 10))
                false
                "Returns false"))
(define (com-on-lst aop alst)
  (cmp-operate (comop->fn aop) 0 (sub1 (length alst)) alst))

; Any Natural Natural [ListOf<Number> or ListOf<Boolean>] -> 
; [Number or Boolean]
; Operates operator op on list lst by using divide-and-conquer
; Strategy: Generative Recursion
; HALTING MEASURE: low always increases or hi always decreases until equal
(begin-for-test
  (check-equal? (arith/bool-operate + 0 1 '(1 2))
                3
                "1 + 2 = 3"))
(define (arith/bool-operate op low hi lst)
  (local ((define mid (floor (/ (+ low hi) 2))))
    (cond
      [(= low hi) (list-ref lst hi)]
      [else (op (arith/bool-operate op low mid lst)
                (arith/bool-operate op (add1 mid) hi lst))])))

; Any Natural Natural ListOf<Natural> -> Boolean
; Operate operator op on list lst by using divid-and-conquer
; Strategy: Generative Recursion
; HALTING MEASURE: low always increases or hi always decreases until equal
(begin-for-test
  (check-true (cmp-operate < 0 2 '(1 2 3))
                "1 < 2 < 3"))
(define (cmp-operate op low hi lst)
  (local ((define mid (floor (/ (+ low hi) 2))))
    (cond 
      [(= (- hi low) 1) (op (list-ref lst low) (list-ref lst hi))]
      [else (and (cmp-operate op low mid lst)
                 (cmp-operate op mid hi lst))])))

; lambda? : Lambda -> Boolean
; Returns true if lam is a PDPLang lambda expression.
(begin-for-test
  (check-true (lambda? (make-lam '(x y) 'x))))
; STRATEGY : Function composition
(define (lambda? l)
  (lam? l))

; errstr? : Expr -> Boolean
; Returns true if e is a PDPLang ErrString expression.
; STRATEGY : Function composition
(begin-for-test
(check-true
 (ormap
  errstr?
  (eval (list (make-arith '+ (list 2 true))
              (make-bool 'and (list 2 true))
              (make-if-exp 3 1 2))))))
(define (errstr? e)
  (string? e))

; expr->expr/no-var : Expr -> ExprNoVar
; Replaces Var in expr0 with StaticDist.
; WHERE: there are no unbound variables in expr0.
; STRATEGY : Function composition
(begin-for-test
  (check-equal?
   (expr->expr/no-var (make-lam '(x) 'x))
   (make-lam/no-var '(0 0))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-arith '+ '(y z)))))
   (make-lam/no-var (make-lam/no-var 
                     (make-arith '+ (list (list 1 1) (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-cmp '> '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-cmp '> (list (list 1 1) 
                                                        (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) 
                                                 (make-bool 'and '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-bool 'and (list (list 1 1) 
                                                           (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) 
                                                 (make-bool 'and '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-bool 'and (list (list 1 1) 
                                                           (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var 
    (make-lam '(x y) (make-lam '(z) (make-if-exp (make-cmp '> '(2 1)) 10 20))))
   (make-lam/no-var (make-lam/no-var (make-if-exp 
                                      (make-cmp '> (list 2 1)) 10 20)))
   "nested lambdas"))
(define (expr->expr/no-var expr0) 
  (local (
          ;expr->expr/no-var-on-lst : ListOf<Expr> ListOf<Expr> -> 
          ;ListOf<ExprNoVar>
          ;Replaces Var in lst with StaticDist for every item in the list 
          ;Strategy: function composition
          (define (expr->expr/no-var-on-lst lst statdis-map)
            (map (lambda (e) (expr->expr/no-var/a e statdis-map))
                 lst))
          ;expr->expr/no-var/a: Expr ListOf<Expr> -> ExprNoVar
          ;Converts expr to its no-var format
          ;statdis-so-far is a list of StaticDist that have been computed in 
          ;expr0 so far and expr is the expression to be converted from expr0.
          ;Strategy: data decompostion on expr : Expr
          (define (expr->expr/no-var/a expr statdis-so-far)
            (cond
              [(or (number? expr) (boolean? expr) (string? expr)) expr]
              [(symbol? expr) (retrieve-map expr statdis-so-far)]
              [(lam? expr) 
               (make-lam/no-var 
                (expr->expr/no-var/a (lam-body expr) 
                                     (update-map (lam-params expr)
                                                 statdis-so-far)))]
              [(arith? expr) (make-arith (arith-op expr)
                                         (expr->expr/no-var-on-lst 
                                          (arith-args expr) statdis-so-far))]
              [(bool? expr) (make-bool (bool-op expr)
                                       (expr->expr/no-var-on-lst 
                                        (bool-args expr) statdis-so-far))]
              [(cmp? expr) (make-cmp (cmp-op expr)
                                       (expr->expr/no-var-on-lst 
                                        (cmp-args expr) statdis-so-far))]
              [(if-exp? expr) (make-if-exp (expr->expr/no-var/a 
                                            (if-exp-test expr) 
                                            statdis-so-far)
                                           (expr->expr/no-var/a 
                                            (if-exp-branch1 expr)  
                                            statdis-so-far)
                                           (expr->expr/no-var/a 
                                            (if-exp-branch2 expr)  
                                            statdis-so-far))]
              [else (make-call (expr->expr/no-var/a (call-fn expr) 
                                                    statdis-so-far)
                               (expr->expr/no-var-on-lst 
                                        (call-args expr) statdis-so-far))])))
    (expr->expr/no-var/a expr0 '())))

; retrive-map : Var ListOf<Expr> -> StaticDist
; Returns the StaticDist for a particular key
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (retrieve-map 'x (list (list 'x '(0 1)) (list 'y '(0 0))))
                (list 0 1)
                "Retreived staticdist for 'x"))
(define (retrieve-map key map)
  (second (first (filter (lambda (x) (eq? key (first x)))
                  map))))

; update-map : ListOf<Var> ListOf<Expr> -> ListOf<Expr>
; Adds a new var to the list, updates the depth and index of existing
; variables
; STRATEGY : Function composition
(begin-for-test
  (check-equal? (update-map (list 'z) (list (list 'x '(0 1)) (list 'y '(0 0))))
                (list
                 (list 'z (list 0 0))
                 (list 'x (list 1 1))
                 (list 'y (list 1 0)))
                "Creates new z entry"))
(define (update-map keys map)
  (local (; add-1 : Expr -> Expr
          ; Adds 1 to the depth of a particular Var
          ; STRATEGY : function composition
          (define (add-1 e)
            (list (first e) 
                  (list (add1 (first (second e)))
                        (second (second e)))))
          ; add1-dis : ListOf<Expr> -> ListOf<Expr>
          ; Adds 1 to the depth of every single Var in the list
          ; STRATEGY : Data decomposition on map : ListOf<Expr>
          (define (add1-dis map)
            (cond 
              [(empty? map) '()]
              [else (cons (add-1 (first map)) (add1-dis (rest map)))]))
          ; add-params : ListOf<Var> ListOf<Expr> -> ListOf<Expr>
          ; Adds a new var to the given list of Expr
          ; STRATEGY : Function composition
          (define (add-params keys map)
            (foldr (lambda (key lst) (cons (list key (list 0 (index key keys)))
                                           lst))
                    map
                    keys)))
    (add-params keys (add1-dis map))))

; index: Var ListOf<Var> -> Natural
; Returns the 0-based position of key in list array
; STRATEGY : Data decomposition on array : ListOf<Var>
(begin-for-test
  (check-equal? (index 'x (list 'x 'y))
                0
                "Computed index for 'x")
  (check-equal? (index 'z '())
                -1
                "Empty array"))
(define (index key array)
  (cond
    [(empty? array) -1]
    [else (if (eq? key (first array))
              0
              (add1 (index key (rest array))))]))

; expr=? : Expr Expr -> Boolean
; Returns true if e1 and e2 are structurally equivalent, up to some
; renaming of variable names.
; STRATEGY : Function composition
(begin-for-test
  (check
   expr=?
   (make-lam '(x) 'x)
   (make-lam '(y) 'y)
   "equivalent basic lambdas")
  (check-false
   (expr=?
    (make-lam '(x y) (make-call 'x '(y)))
    (make-lam '(y x) (make-call 'x '(y))))
   "not equivalent")
  (check
   expr=?
   (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))
   (make-lam '(x) (make-lam '(y) (make-call 'x '(y))))
   "equivalent nested-lambdas"))
(define (expr=? e1 e2)
  (equal? (expr->expr/no-var e1) (expr->expr/no-var e2)))