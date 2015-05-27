#lang racket
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 10)

(provide mk-Program%)
(provide mk-Expr%)
(provide Program<%>)
(provide Expr<%>)
(provide Result<%>)

; A UniqueListOf<X> is a ListOf<X>
; WHERE: none of the elements of the list are equal? to each other
(define ulst '(1 2 3))
;Template: 
;STRATEGY : Data decomposition on l : UniqueListOf<X>
;(define (unique-list-fn l)
;  (cond
;    [(empty? l) ...]
;    [else
;     (...(first l) ... (unique-list-fn (rest l)))]))

; A 2ListOf<X> is a (cons X (cons X ListOf<X>))
; Represents a list of 2 or more elements.
(define 2lox '(1 2))
; Template:
; 2ListOf<X> -> ???
; STRATEGY: data decomposition on 2lox : 2ListOf<X>
; WHERE: X is Expr or ExprNoVar
;(define (2lox-fn 2lox)
;  (... (x-fn (first 2lox)) ...
;   ... (x-fn (second 2lox))... 
;   ...(list-fn (rest (rest 2lox)))...))

; A Program is a:
; - empty
; - (cons Expr Program)
; - (cons Def Program)
; Represents a PDPLang program consisting of function defs and expressions.
; WHERE: no two Defs have the same name 
; Template:
; program-fn :Program -> ???
; STRATEGY: data decomposition on p : Program
;(define (program-fn p)
;  (cond 
;    [(empty? p) ...]
;    [(not (def? (first p))) 
;     (...(expr-fn (first p))... (program-fn (rest p))...)]
;    [else (...(def-fn (first p))... (program-fn (rest p))...)]))

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
;(define (expr-fn expr)
;  (cond
;    [(number? expr) ...]
;    [(boolean? expr) ...]
;    [(symbol? expr) ...]
;    [(string? expr) ...]
;    [(lam? expr) (...(lambda-fn expr)...)]
;    [(arith? expr) (...(arithop-fn (arith-op expr))... 
;                      ...(2lox-fn (arith-args expr))...)]
;    [(bool? expr) (...(boolop-fn (bool-op expr))... 
;                       ...(2lox-fn (bool-args expr))...)]
;    [(cmp? expr) (...(cmpop-fn (cmp-op expr))... 
;                       ...(2lox-fn (cmp-args expr))...)]
;    [(if-exp? expr) (...(expr-fn (if-exp-test expr))...
;                        ...(expr-fn (if-exp-branch1 expr))...
;                        ...(expr-fn (if-exp-branch2 expr))...)]
;    [else (...(expr-fn (call-fn expr))...
;           ...(list-fn (call-args expr)...))]))

(define-struct arith (op args) #:transparent)
; An Arith is a (make-arith ArithOp 2ListOf<Expr>)
; Represents an arithmatic operation which contains an arithmatic
; operator op and two or more operands args
(define a1 (make-arith '+ '(1 2)))
; Template:
; arith-fn :Arith -> ???
; STRATEGY : Data decomposition on a : Arith
;(define (arith-fn a)
;  (... (arithop-fn (arith-op a)) ... (2lox-fn (arith-args a))))

(define-struct bool (op args) #:transparent)
; A Bool is a (make-bool BoolOp 2ListOf<Expr>)
; Represents a boolean operation which contains a boolean operator op
; and two or more operands args
(define aa1 (make-bool 'and '(#true #false)))
; Template:
; bool-fn :Bool -> ???
; STRATEGY : Data decomposition on b : Bool
;(define (bool-fn b)
;  (... (boolop-fn (bool-op b)) ... (2lox-fn (bool-args b))))

(define-struct cmp (op args) #:transparent)
; A Cmp is a (make-cmp CmpOp 2ListOf<Expr>)
; Represents a comparison operation which contains a comparison operator op
; and two or more operands args
(define b1 (make-cmp '> '(1 2)))
; Template:
; cmp-fn : Cmp -> ???
;STRATEGY : Data decomposition on c : Cmp
;(define (cmp-fn a)
;  (... (cmpop-fn (cmp-op c)) ... (2lox-fn (cmp-args c))))

(define-struct if-exp (test branch1 branch2) #:transparent)
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
;(define (if-exp-fn i)
;  (... (expr-fn (if-exp-test i)) ... (expr-fn (if-exp-branch1 i))
;       ... (expr-fn (if-exp-branch2 i))))

(define-struct call (fn args) #:transparent)
; A Call is a (make-call Expr ListOf<Expr>)
; Represents a function call with a function name fn and parameters args
; Template :
(define c1 (make-call 'x '(1 2)))
; call-func :Call -> ???
; STRATEGY : Data decomposition on c : Call
;(define (call-func c)
;  (... (expr-fn (call-fn c)) ... (list-fn((call-args c))))

; A Var is a Symbol, representing PDPLang variable.
(define v1 'x)

; An ErrString is a String, representing a PDPLang error message.
(define err1 "err: test failed")

(define-struct lam (params body) #:transparent)
; A Lambda is a (make-lam UniqueListOf<Param> Expr)
; Represents a lambda expression in PDPLang
(define lam1 (make-lam '(x y) 'y))
; Template:
; lambda-fn :Lam -> ???
;STRATEGY : Data decomposition on l : Lambda
;(define (lambda-fn l)
;  ( .... (unique-list-fn (lambda-params l) ... (expr-fn (lambda-body)))))

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
;(define (arithop-fn a)
;  (cond
;    [(symbol=? ADD a) ...]
;    [(symbol=? SUB a) ...]
;    [(symbol=? MUL a) ...]
;    [(symbol=? DIV a) ...]))

; A BoolOp is one of:
; - 'and
; - 'or
; Represents a boolean operation in PDPLang
(define AND 'and)
(define OR 'or)
; Template :
; boolop-fn: BoolOp -> ???
;STRATEGY: Data decomposition on b : BoolOp
;(define (boolop-fn b)
;  (cond
;    [(symbol=? AND b) ...]
;    [(symbol=? OR b) ...]))

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
;(define (cmpop-fn c)
;  (cond
;    [(symbol=? EQUAL c) ...]
;    [(symbol=? LT c) ...]
;    [(symbol=? GT c) ...]))

(define-struct def (name params body) #:transparent)
; A Def is a (make-def FnName UniqueListOf<Param> Expr)
; Represents the definition of a function with the specified parameters.
(define deff1 (make-def 'fx '(x) 'x))
; Template:
; def-fn : Def -> ???
; STRATEGY: Data decomposition on d : Def
;(define (def-fn d)
;  (... (def-name d) ... (unique-list-fn (def-params d)) ... 
;       (expr-fn (def-body d))))

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
;(define (result-fn r)
;  (cond
;    [(number? r) ...]
;    [(boolean? r) ...]
;    [(string? r) ...]
;    [(lam? r) (...(lambda-fn r)...)]))

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
;(define (expr-no-var-fn expr-no-var)
;  (cond
;    [(number? expr-no-var) ...]
;    [(boolean? expr-no-var) ...]
;    [(list? expr-no-var) (...(static-dist-fn expr) ...)]
;    [(string? expr-no-var) ...]
;    [(lam/no-var? expr-no-var) (...(lam/no-var-fn expr-no-var)...)]
;    [(arith? expr-no-var) (...(arithop-fn (arith-op expr-no-var))... 
;                       ...(2lox-fn (arith-args expr-no-var))...)]
;    [(bool? expr-no-var) (...(boolop-fn (bool-op expr-no-var))... 
;                      ...(2lox-fn (bool-args expr-no-var))...)]
;    [(cmp? expr-no-var) (...(cmpop-fn (cmp-op expr-no-var))... 
;                       ...(2lox-fn (cmp-args expr-no-var))...)]
;    [(if-exp? expr-no-var) (...(expr-no-var-fn (if-exp-test expr-no-var))...
;                        ...(expr-no-var-fn (if-exp-branch1 expr-no-var))...
;                        ...(expr-no-var-fn (if-exp-bramch2 expr-no-var))...)]
;    [else (...(expr-no-var-fn (call-fn expr-no-var))...
;           ...(list-fn (call-args expr-no-var)...))]))

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
;(define (static-dist-fn s)
;  (...(first s) ... (second s)))

; A Depth is a Natural
(define d1 1)

; An Index is a Natural
(define i1 0)

; A LamNoVar is a (make-lam/no-var ExprNoVar)
(define-struct lam/no-var (body) #:transparent)
; Represents a lambda expression in PDPLang without explicit variables
(define lnv1 '(1 0))
; Template
; lam/no-var -> ???
; STRATEGY : Data decomposition on lnv : lam/no-var
;(define (lam/no-var-fn lnv)
;  (...(expr-no-var-fn (lam/no-var-body lnv))...))

(define Evaluate<%>
  (interface ()
    ; expr->result : Expr ListOf<Def> -> Result
    ; Evaluates the Expr in context of the defs
    expr->result
    
    ; subst : Expr Result Var -> Expr
    ; Replaces references in Expr to the given var with the given Result
    ; Does not replace x with Result if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result has no unbound variables.
    subst
    
    ; expr->expr/no-var : Expr -> ExprNoVar
    ; Replaces Var in Expr with StaticDist.
    ; WHERE: there are no unbound variables in Expr.
    expr->expr/no-var
    
    ; expr=? : Expr Expr -> Boolean
    ; Returns true if the two Expr are structurally equivalent, up to some
    ; renaming of variable names.
    expr=?
    
    ; eval : Program -> ListOf<Result>
    ; Evaluates a PDPLang program to a list of Results.
    ; Specifically, evaluates the Exprs in p, in the context of the given Defs.
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The produced results are in the same relative order as their 
    ; originating Exprs in p.
    eval))

; Evaluater% : A class that satisfies the Evaluate<%>
;             interface
; A Evaluater% is a (new Evaluater%)
; INTERP : This class have no field.
(define Evaluater%
  (class* object% (Evaluate<%>)
    (init-field)
    
    ; eval : Program -> ListOf<Result>
    (define/public (eval p)
      (local ((define defs (filter-defs p)))
        (eval-with-defs p defs)))
    
    ; eval-with-defs : Program ListOf<Def> -> ListOf<Result>
    ; Evaluates the Program, p in context of the defs
    ; STRATEGY : Data decomposition on p : Program
    (define (eval-with-defs p defs)
      (cond 
        [(empty? p) '()]
        [(not (def? (first p))) 
         (cons (expr->result (first p) defs) (eval-with-defs (rest p) defs))]
        [else (eval-with-defs (rest p) defs)]))
    
    ; filter-defs : Program -> ListOf<Def>
    ; Seperates out the defs from Program
    ; STRATEGY : Data decomposition on p : Program
    (define (filter-defs p)
      (cond
        [(empty? p) '()]
        [(not (def? (first p))) (filter-defs (rest p))]
        [else (cons (first p) (filter-defs (rest p)))]))
    
    ; expr->result : Expr ListOf<Def> -> Result
    ; STRATEGY : Generative Recursion
    ; TERMINATE ARGUMENT: this function will not terminate if there exists
    ; inifite loop in eval-call, e.g: f(x) = f(x) + 1 will make eval-call never 
    ; terminate.
    (define/public (expr->result expr defs)
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
        (if (lam? subfn)
            (eval0 subfn)
            "err: subfn is NOT lambda in call")))
    
    ; mul-subst : ListOf<Result> ListOf<Expr> Expr -> Expr
    ; Replaces every reference to every item to args0 in expr0 with 
    ; the corresponding item in results0
    (define (mul-subst results0 args0 expr0)
      (local ((define compatable? (= (length results0) (length args0)))
              ;ListOf<Result> ListOf<Expr> Expr -> Expr
              ;Replaces every reference to every item to args in expr with 
              ;the corresponding item in results
              ;WHERE: expr-replace-so-far is the expr that has been replaced so 
              ;far, the results is the elements in results0 to replace and args 
              ;is the elements in args0 to be replaced by results 
              ;correspondingly.
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
    (define/public (subst r x expr) 
      (cond 
        [(or (number? expr) (boolean? expr) (string? expr)) expr]
        [(symbol? expr) (if (eq? expr x) r expr)]
        [(lam? expr) (if (list? (member x (lam-params expr)))
                         expr
                         (make-lam (lam-params expr) 
                                   (subst r x (lam-body expr))))] 
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
    (define (subst-in-lst r x args)
      (map (lambda (e) (subst r x e))
           args))
    
    ; eval-symbol: Var List<Def> -> Lambda
    ; Evaluates a symbol
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
    (define (loex->lore lox defs)
      (map (lambda (e) (expr->result e defs)) lox))
    
    ; eval-operation: [ArithOp or BoolOp or ComOp] 2ListOf<Result> -> Result
    ; Evaluates all arithmatic, boolean and comparison operations
    (define (eval-operation op lst)
      (if (compatable? lst)
          (operate-on-lst op lst)
          "err: incompatable between operator and args"))
    
    ; operate-on-lst : [ArithOp or BoolOp or ComOp] 2ListOf<Result> -> Result
    ; Returns the result after evaluating the operation
    (define (operate-on-lst op lst)
      (if (arithop? op)
          (arith-on-lst op lst)
          (if (cmpop? op)
              (com-on-lst op lst)
              (bool-on-lst op lst))))
    
    ; arithop? : Symbol -> Boolean
    ; Returns true if the operation is a ArithOp
    (define (arithop? op)
      (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '/)))
    
    ; cmpop? : Symbol -> Boolean
    ; Returns true if the operation is a CmpOp
    (define (cmpop? op)
      (or (eq? op '>) (eq? op '=) (eq? op '<)))
    
    ; operate-bool-on-lst : BoolOp 2ListOf<Result> -> Result
    ; Evaluates all boolean operations to a Result
    (define (bool-on-lst op lst)
      (arith/bool-operate (boolop->fn op) 0 (sub1 (length lst)) lst))
    
    ; compatable? :ListOf<Result> -> Boolean
    ; Returns true if the list is a list of numbers or list of boolean
    (define (compatable? lst)
      (local (; one-type? : ListOf<Result> -> Boolean
              ; Returns true if the list is a list of numbers or list of 
              ; boolean
              ; STRATEGY : Function composition
              (define (one-type? lst)
                (or (andmap (lambda (r) (number? r))
                            lst)   
                    (andmap (lambda (r) (boolean? r))
                            lst))))
        (one-type? lst)))
    
    ; arithop->fn : ArithOp -> [X X -> X]
    ; Returns the operator for it's corresponding symbol 
    ; STRATEGY : Data decomposition on aop : ArithOp
    (define (arithop->fn aop)
      (cond 
        [(eq? aop '+) +]
        [(eq? aop '-) -]
        [(eq? aop '*) *]
        [(eq? aop '/) /]))
    
    ; comop->fn : CmpOp -> [X X -> X]
    ; Returns the operator for it's corresponding symbol 
    ; STRATEGY : Data decomposition on aop : ComOp
    (define (comop->fn aop)
      (cond 
        [(eq? '> aop) >]
        [(eq? '= aop) =]
        [(eq? '< aop) <]))
    
    ; boolop->fn: BoolOp -> [X X -> X]
    ; Returns the operator for it's corresponding symbol 
    ; STRATEGY : Data decomposition on aop : BoolOp
    (define (boolop->fn aop)
      (cond
        [(eq? aop 'and) (lambda (x y) (and x y))]
        [(eq? aop 'or) (lambda (x y) (or x y))]))
    
    ; arith-on-lst : ArithOp 2ListOf<Result> -> Result
    ; Evaluates arithmatic operations
    ; STRATEGY : function composition
    (define (arith-on-lst aop alst)
      (local ((define divide-by-zero? (and (eq? / (arithop->fn aop)) 
                                           (ormap (lambda (x) (zero? x))
                                                  (rest alst)))))
        (if divide-by-zero?
            "err: divide-by-zero"
            (arith/bool-operate (arithop->fn aop) 0 
                                (sub1 (length alst)) alst))))
    
    ; com-on-lst : 2ListOf<Result> -> Result
    ; Evaluates the comparison operations
    ; STRATGY : function composition
    (define (com-on-lst aop alst)
      (cmp-operate (comop->fn aop) 0 (sub1 (length alst)) alst))
    
    ; [X X -> X] Natural Natural [ListOf<Number> or ListOf<Boolean>] -> 
    ; [Number or Boolean]
    ; Operates operator op on list lst by using divide-and-conquer
    ; Strategy: Generative Recursion
    ; HALTING MEASURE: low always increases or hi always decreases until equal
    (define (arith/bool-operate op low hi lst)
      (local ((define mid (floor (/ (+ low hi) 2))))
        (cond
          [(= low hi) (list-ref lst hi)]
          [else (op (arith/bool-operate op low mid lst)
                    (arith/bool-operate op (add1 mid) hi lst))])))
    
    ; [X X -> X] Natural Natural ListOf<Natural> -> Boolean
    ; Operate operator op on list lst by using divid-and-conquer
    ; Strategy: Generative Recursion
    ; HALTING MEASURE: low always increases or hi always decreases until equal
    (define (cmp-operate op low hi lst)
      (local ((define mid (floor (/ (+ low hi) 2))))
        (cond 
          [(= (- hi low) 1) (op (list-ref lst low) (list-ref lst hi))]
          [else (and (cmp-operate op low mid lst)
                     (cmp-operate op mid hi lst))])))
    
    ; expr->expr/no-var : Expr -> ExprNoVar
    ; Replaces Var in expr0 with StaticDist.
    ; WHERE: there are no unbound variables in expr0
    (define/public (expr->expr/no-var expr0) 
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
              ;statdis-so-far is a list of StaticDist that have been computed 
              ;in expr0 so far and expr is the expression to be converted from 
              ;expr0.
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
                                              (arith-args expr) 
                                              statdis-so-far))]
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
    (define (retrieve-map key map)
      (second (first (filter (lambda (x) (eq? key (first x))) map))))
    
    ; update-map : ListOf<Var> ListOf<Expr> -> ListOf<Expr>
    ; Adds a new var to the list, updates the depth and index of existing
    ; variables
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
                (foldr (lambda (key lst) (cons (list key 
                                                     (list 0 (index key keys)))
                                               lst))
                       map
                       keys)))
        (add-params keys (add1-dis map))))
    
    ; index: Var ListOf<Var> -> Natural
    ; Returns the 0-based position of key in list array
    ; STRATEGY : Data decomposition on array : ListOf<Var>
    (define/public (index key array)
      (cond
        [(empty? array) -1]
        [else (if (eq? key (first array))
                  0
                  (add1 (index key (rest array))))]))
    
    ; expr=? : Expr Expr -> Boolean
    ; Returns true if e1 and e2 are structurally equivalent, up to some
    ; renaming of variable names.
    (define/public (expr=? e1 e2)
      (equal? (expr->expr/no-var e1) (expr->expr/no-var e2)))
    (super-new)))

; Constant EVALUATER
(define EVALUATER (new Evaluater%))

(define Result<%>
  (interface ()
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    app))

(define Expr<%>
  (interface ()
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    eval
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    subst
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    to-expr
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    to-expr/no-var
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    expr=?))

; Number% : A class that satisfies the Expr<%> Result<%> interface
; A Number% is a (new Number% [value Number])
; INTERP : Represents a number, with a value represents the actual number
(define Number%
  (class* object% (Expr<%> Result<%>)
    (init-field value)
    ; INTERP: value represents the actual value
    
    ; eval : ListOf<Def> -> Number%
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Number%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define/public (app results defs)
      (error "Tring to use app on a Number!"))
    
    (super-new)))

; Boolean% : A class that satisfies the Expr<%> Result<%> interface
; A Boolean% is a (new Boolean% [value Boolean])
; INTERP : Represents a Boolean, with a value represents the actual #f or #t
(define Boolean%
  (class* object% (Expr<%> Result<%>)
    (init-field value)
    ; INTERP: value represents the actual value
    
    ; eval : ListOf<Def> -> Boolean%
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Boolean%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Boolean
    (define/public (to-expr)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Boolean%
    (define/public (app results defs)
      (error "Tring to use app on a Boolean!"))
    
    (super-new)))

; ErrString% : A class that satisfies the Expr<%> Result<%> interface
; A ErrString% is a (new ErrString% [value Boolean])
; INTERP : Represents a ErrString, with a value represents the actual ErrString
(define ErrString%
  (class* object% (Expr<%> Result<%>)
    (init-field value)
    ; INTERP : value represents the actual ErrString
    
    ; eval : ListOf<Def> -> ErrString%
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> ErrString%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> ErrString%
    (define/public (app results defs)
      (error "Tring to use app on a ErrString!"))
    
    (super-new)))

; Var% : A class that satisfies the Expr<%> interface
; A Var% is a (new Var% [value Var])
; INTERP : Represents a Var, with a value represents the actual Var
(define Var%
  (class* object% (Expr<%>)
    (init-field value)
    ; INTERP : value represents the actual Var
    
    ; eval : ListOf<Def> -> Var%
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Var%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      ;(send EVALUATER expr->expr/no-var (to-expr)))
      "error: this method should never be called")
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (equal? value (send expr to-expr)))
    
    (super-new)))

; Lambda% : A class that satisfies the Expr<%> interface
; A Lambda% is a (new Lambda% [params UniqueListOf<Param>] [body Expr])
; INTERP : Represents a Lambda, with a parameter list, function body
(define Lambda%
  (class* object% (Expr<%> Result<%>)
    (init-field params body)
    ; INTERP: params is a the parameter list of the lambda, body is the 
    ; function body of the lambda
    
    ; eval : ListOf<Def> -> Lambda%
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Lambda%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-lam params body))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define/public (app results defs)
      (mk-Result% (send EVALUATER expr->result 
                        (make-call (make-lam params body) results) 
                        defs)))
    
    (super-new)))

; Arith% : A class that satisfies the Expr<%> interface
; A Arith% is a (new Arith% [op ArithOp] [args 2ListOf<Expr>])
; INTERP : Represents a Arith, with a operator, a 2-list of operand
(define Arith%
  (class* object% (Expr<%>)
    (init-field op args)
    ; INTERP: op represents the operator, args representst a 2-list of operand
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Arith%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-arith op args))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    (super-new)))

; Bool% : A class that satisfies the Expr<%> interface
; A Bool% is a (new Bool% [op BoolOp] [args 2ListOf<Expr>])
; INTERP : Represents a Bool, with a operator, a 2-list of operand
(define Bool%
  (class* object% (Expr<%>)
    (init-field op args)
    ; INTERP: op represents the operator, args representst a 2-list of operand
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Bool%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-bool op args))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    (super-new)))

; Cmp% : A class that satisfies the Expr<%> interface
; A Cmp% is a (new Cmp% [op CmpOp] [args 2ListOf<Expr>])
; INTERP : Represents a Cmp, with a operator, a 2-list of operand
(define Cmp%
  (class* object% (Expr<%>)
    (init-field op args)
    ; INTERP: op represents the operator, args representst a 2-list of operand
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Cmp%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-bool op args))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    (super-new)))

; If-exp% : A class that satisfies the Expr<%> interface
; A If-exp% is a (new If-exp% [test Expr] [branch1 Expr][branch2 Expr])
; INTERP : Represents a If-exp, with a test condition, two braches
(define If-exp%
  (class* object% (Expr<%>)
    (init-field test branch1 branch2)
    ; INTERP : test represents the test condition, if test is true, goes to
    ; branch1, otherwise branch2
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> If-exp%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-if-exp test branch1 branch2))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    (super-new)))

; Call% : A class that satisfies the Expr<%> interface
; A Call% is a (new Call% [fn Expr] [args ListOf<Expr>])
; INTERP : Represents a Call, with a fn, args
(define Call%
  (class* object% (Expr<%>)
    (init-field fn args)
    ; INTERP : fn represents the calle function name, args represents the 
    ; parameter list
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval defs)
      (mk-Result% (send EVALUATER expr->result (to-expr) defs)))
    
    ; subst : Result<%> Var -> Call%
    (define/public (subst result var)
      (mk-Expr% (send EVALUATER subst (send result to-expr) var (to-expr))))
    
    ; to-expr : -> Expr
    (define/public (to-expr)
      (make-call fn args))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var)
      (send EVALUATER expr->expr/no-var (to-expr)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr)
      (send EVALUATER expr=? (to-expr) (send expr to-expr)))
    
    (super-new)))

(define Program<%>
  (interface ()
    ; eval : -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in 
    ; the program.
    eval
    
    ; test:eval->expr : -> ListOf<Result>
    ; Evaluates expressions in the program to ListOf<Result>
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in 
    ; the program.
    test:eval->expr))

; ProgramObj% : A class that satisfies the Program<%> interface
; A ProgramObj% is a (new ProgramObj% [program Program])
; INTERP : Represents a Program, with a program
(define ProgramObj%
  (class* object% (Program<%>)
    (init-field program)
    ; INTERP : program represents a PDPLang program consisting of function 
    ; defs and expressions.
    
    ; eval : -> ListOf<Result<%>>
    (define/public (eval)
      (map (lambda (r) (mk-Result% r))
           (test:eval->expr)))
    
    ; test:eval->expr : -> ListOf<Result>
    (define/public (test:eval->expr)
      (send EVALUATER eval program))
    
    (super-new)))

; mk-Program% : Program -> Program<%>
; Returns a Program% obj representing the give Program p
; Strategy: function composition
(begin-for-test 
  (check-true (object? (mk-Program% P) )))
(define (mk-Program% p) 
  (new ProgramObj% [program p]))

; mk-Expr% : Expr -> Expr<%>
; Returns a Expr<%> obj representing the give Expr expr
; Strategy: Data decomposition on expr: Expr
(begin-for-test
  (check-true (object? (mk-Expr% 1))))
(define (mk-Expr% expr) 
  (cond
    [(number? expr) (new Number% [value expr])]
    [(boolean? expr) (new Boolean% [value expr])]
    [(symbol? expr) (new Var% [value expr])]
    [(string? expr) (new ErrString% [value expr])]
    [(lam? expr) (new Lambda% [params (lam-params expr)] 
                      [body (lam-body expr)])]
    [(arith? expr) (new Arith%[op (arith-op expr)] 
                        [args (arith-args expr)])]
    [(bool? expr) (new Bool%[op (bool-op expr)] 
                       [args (bool-args expr)])]
    [(cmp? expr) (new Cmp%[op (cmp-op expr)] 
                      [args (cmp-args expr)])] 
    [(if-exp? expr) (new If-exp%[test (if-exp-test expr)]
                         [branch1 (if-exp-branch1 expr)]
                         [branch2 (if-exp-branch2 expr)])]
    [else (new Call% [fn (call-fn expr)] 
               [args (call-args expr)])]))

; mk-Result% : Result -> Result<%>
; Returns a Result<%> obj representing the give Result<%> r
; Strategy: Data decomposition on r : Result
(define (mk-Result% r)
  (cond
    [(number? r) (new Number% [value r])]
    [(boolean? r) (new Boolean% [value r])]
    [(string? r) (new ErrString% [value r])]
    [(lam? r) (new Lambda%[params (lam-params r)] 
                   [body (lam-body r)])]))

; Data Examples
(define A1 (make-arith '/ (list 15 5)))
(define A2 (make-arith '* (list A1 9)))
(define A3 (make-arith '+ (list 998 #true)))
(define A4 (make-arith '- (list 998 1)))
(define A5 (make-arith '/ (list 998 0)))
(define C1 (make-cmp 'or (list #true #false #true #false #true #false)))
(define C2 (make-cmp 'and (list #true #false #true #false #true #false)))
(define C3 (make-cmp '< (list 1 2 3 4 5 6 7)))
(define B (make-bool 'and (list C1 C2 #false)))
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
(define IF-EXPR4 (make-if-exp 10 10 10))
(define LAM (make-lam '(x y) (make-arith '+ '(x y))))
(define LAM2 (make-lam '(y) (make-lam '(x) (make-call 'y '(x)))))
(define LAM3 (make-lam '(x y) (make-call 'x '(y))))
(define LAM4 (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
(define LAM5 (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
(define LAM6 (make-lam '(x y) (make-lam '(z) (make-arith '+ '(y z)))))
(define LAM7 (make-lam '(x y) (make-lam '(z) (make-cmp '> '(y z)))))
(define LAM8 (make-lam '(x y) (make-lam '(z) 
                                        (make-bool 'and '(y z)))))
(define LAM9 (make-lam '(x y) (make-lam '(z) 
                                        (make-bool 'and '(y z)))))
(define LAM10 (make-lam '(x y) (make-lam '(z) (make-if-exp 
                                               (make-cmp '> '(2 1)) 10 20))))

(define F (make-def 'f (list 'x 'y 'z) 
                    (make-if-exp (make-cmp '> (list 'x 'y))
                                 (make-call 'g (list 'x 'y 'z))
                                 (make-arith '* (list 'x 'y 'z)))))

(define G (make-def 'g 
                    (list 'x 'y 'z)
                    (make-arith '+ (list 'x 'y 'z))))

(define CALL1 (make-call 'f (list 8 4 2)))
(define CALL2 (make-call (make-lam '(x) 'x) '(1))) 
(define CALL3 (make-call 'f (list 8 4 2 3)))
(define CALL4 (make-call (make-cmp '> (list A1 10)) (list 8 4 2 3)))
(define P 
  (list A1 A2 A3 A5 A4 C1 C2 C3 G IF-EXPR1 IF-EXPR2 'f 'g IF-EXPR4
        10 "err: test-err" F #false LAM CALL1 CALL3 CALL4))

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

(define P/Obj (mk-Program% P))

(define LISTEXPR 
  (list A1 C1 IF-EXPR1 IF-EXPR2 10 "err: test-err" #false LAM B))

(define LISTEXPR2 
  (list 'x 'g A1 A2 A3 A4 C1 C2 B IF-EXPR1 IF-EXPR2 CALL2 
        10 "err: test-err" #false LAM LAM2 LAM3 LAM4 LAM5 LAM6 LAM7 LAM8))

(define LISTEXPR% (map (lambda (e) (mk-Expr% e)) LISTEXPR))

(define LISTEXPR2% (map (lambda (e) (mk-Expr% e)) LISTEXPR2))

(define LISTRESULT (list 0 "ERR" #true LAM))

(define LISTRESULT% (map (lambda (e) (mk-Result% e)) LISTRESULT))

(define LAMS (list LAM2 LAM3 LAM4 LAM5 LAM6 LAM7 LAM8 LAM9 LAM10 CALL2))

(define LAMS% (map (lambda (e) (mk-Expr% e)) LAMS))

(define NUM (new Number% [value 1]))

(define BO (new Boolean% [value #false]))

(define Va (new Var% [value 'x]))

(define ER (new ErrString% [value "err"]))

(define LS (list LAM2 A1 C1 B IF-EXPR1 CALL2))

(define LS% (append (list NUM BO ER) (map (lambda (e) (mk-Expr% e)) LS)))

; EVALUATER Test:
(begin-for-test
  (check-equal? (send EVALUATER index 'z '()) -1))

; Tests:
(begin-for-test 
  (check-equal? (map (lambda (e) (send e expr=? e)) LISTEXPR%)
                '(#t #t #t #t #t #t #t #t #t))
  (check-equal? (send P/Obj test:eval->expr)
                (list
                 3
                 27
                 "err: incompatable between operator and args"
                 "err: divide-by-zero"
                 997
                 #t
                 #f
                 #t
                 #f
                 #f
                 (lam
                  '(x y z)
                  (if-exp (cmp '> '(x y)) (call 'g '(x y z)) 
                          (arith '* '(x y z))))
                 (lam '(x y z) (arith '+ '(x y z)))
                 "err: test result invaild"
                 10
                 "err: test-err"
                 #f
                 (lam '(x y) (arith '+ '(x y)))
                 14
                 "err: incompatable arguments numbers"
                 "err: subfn is NOT lambda in call"))
  (check-equal? (map (lambda (l) (send l expr=? l)) (cons Va LAMS%))
                '(#t #t #t #t #t #t #t #t #t #t #t))
  (check-equal? (map (lambda (l) (send l to-expr/no-var)) LS%)
                (list
                 1
                 #f
                 "err"
                 (lam/no-var (lam/no-var (call '(1 0) '((0 0)))))
                 (arith '/ '(15 5))
                 (bool 'or '(#t #f #t #f #t #f))
                 (bool
                  'and
                  (list
                   (cmp 'or '(#t #f #t #f #t #f))
                   (cmp 'and '(#t #f #t #f #t #f))
                   #f))
                 (if-exp
                  (cmp '= (list (arith '/ '(15 5)) 10))
                  (arith
                   '+
                   (list
                    (arith '/ '(15 5))
                    (arith '* (list (arith '/ '(15 5)) 9))
                    (arith '+ '(998 #t))))
                  (bool
                   'and
                   (list
                    (cmp 'or '(#t #f #t #f #t #f))
                    (cmp 'and '(#t #f #t #f #t #f))
                    #f)))
                 (call (lam/no-var '(0 0)) '(1))))
  (check-equal? (send Va to-expr/no-var)
                "error: this method should never be called")
  (check-true (list? (map (lambda (e) (send e eval DEF)) LISTEXPR2%)))
  (check-true (list? (map (lambda (e) 
                            (send e subst (new Number% [value 0]) 'x)) 
                          LISTEXPR2%)))
  (check-true (list? (map (lambda (e) (send e to-expr)) LISTEXPR2%)))
  (check-true (object? (send (fourth LISTRESULT%) app '(1 2) DEF)))
  (check-error (send (first LISTRESULT%) app '(1 2) DEF))
  (check-error (send (second LISTRESULT%) app '(1 2) DEF))
  (check-error (send (third LISTRESULT%) app '(1 2) DEF))
  
  (check-true (list? (send P/Obj eval)))
  (check-true (object? (mk-Expr% CALL1)))
  (check-true (object? (mk-Expr% 'x))))