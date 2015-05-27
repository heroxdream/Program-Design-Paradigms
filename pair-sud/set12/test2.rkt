#lang racket
(define Boolean%
  (class* object% (Expr<%> Result<%>)
    (init-field value)
    
    ; eval : ListOf<Def> -> Result<%>
    (define (eval/public defs)
      this)
    
    ; subst : Result<%> Var -> Expr<%>
    (define (subst/public result var)
      this)
    
    ; to-expr : -> Expr
    (define (to-expr/public)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define (to-expr/no-var/public)
      value)
    
    ; expr=? : Expr<%> -> Boolean
    (define (expr=?/public expr)
      (expr=? (to-expr/public) (send expr to-expr/public)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define (app/public results defs)
      (error: "Tring to use app on a Boolean!"))
    
    (super-new)))

(define Var%
  (class* object% (Expr<%>)
    (init-field value)
    
    ; eval : ListOf<Def> -> Result<%>
    (define (eval/public defs)
      this)
    
    ; subst : Result<%> Var -> Expr<%>
    (define (subst/public result var)
      (if (eq=? value var) result this))
    
    ; to-expr : -> Expr
    (define (to-expr/public)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define (to-expr/no-var/public)
      '(0 0))
    
    ; expr=? : Expr<%> -> Boolean
    (define (expr=?/public expr)
      (expr=? (to-expr/public) (send expr to-expr/public)))
    
    (super-new)))

(define ErrString%
  (class* object% (Expr<%> Result<%>)
    (init-field value)
    
    ; eval : ListOf<Def> -> Result<%>
    (define (eval/public defs)
      this)
    
    ; subst : Result<%> Var -> Expr<%>
    (define (subst/public result var)
      this)
    
    ; to-expr : -> Expr
    (define (to-expr/public)
      value)
    
    ; to-expr/no-var : -> ExprNoVar
    (define (to-expr/no-var/public)
      value)
    
    ; expr=? : Expr<%> -> Boolean
    (define (expr=?/public expr)
      (expr=? (to-expr/public) (send expr to-expr/public)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define (app/public results defs)
      (error: "Tring to use app on a ErrString!"))
    
    (super-new)))

(define Lambda%
  (class* object% (Expr<%> Result<%>)
    (init-field params body)
    
    ; eval : ListOf<Def> -> Result<%>
    (define (eval/public defs)
      this)
    
    ; subst : Result<%> Var -> Expr<%>
    (define (subst/public result var)
      )
    
    ; to-expr : -> Expr
    (define (to-expr/public)
      ...)
    
    ; to-expr/no-var : -> ExprNoVar
    (define (to-expr/no-var/public)
      ...)
    
    ; expr=? : Expr<%> -> Boolean
    (define (expr=?/public expr)
      (expr=? (to-expr/public) (send expr to-expr/public)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define (app/public results defs)
      )
    
    (super-new)))