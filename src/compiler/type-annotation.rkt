#lang typed/racket/base
;;;; Type Annotations. 

(provide 
    ValidYupianVarDeclar
    ValidYupianFunHeaderDeclar
    (struct-out MiddleVarDeclar)
    (struct-out MiddleFunHeaderDeclar)
    valid-yupian-var-declar?
    valid-yupian-fun-header-declar?
    valid-yupian-var-declar->middle
    valid-yupian-fun-header-declar->middle)

(require racket/list racket/match)

;;; The first thing is the defintion of a type. 
(struct NoExplicitReturnType () #:transparent)

(struct TypeAtom ([t : Any]) #:transparent)

; Okay, Racket understands this.
(define-type TypeExpr (U TypeAtom MapsTo))
(struct MapsTo ([from : TypeExpr] [to : TypeExpr]) #:transparent)

(struct ExplicitReturnType ([t : TypeExpr]) #:transparent)

(define-type ReturnTypeMark
    (U NoExplicitReturnType ExplicitReturnType))

;;; The term works if it fits
;;; * 甲 ' the broadest Value type, implicit
;;; * 『甲 : 型 』 ' the type is 型 --- for the time being, it is unaltered to the Racket layer, 
;;;    so mark it with valid Racket Type
(define-type ValidYupianVarDeclar
	(U  Symbol
		(List Symbol ': Any)))

(define-predicate valid-yupian-var-declar? ValidYupianVarDeclar)

;;; We should support
;;; * A list of terms (can be empty), as a function
;;; And 
;;; * An optional return value type
(define-type ValidYupianReturnDeclar (List* '-> Any (Listof Any)))
(: return-datum->type-expr (-> (Listof Any) TypeExpr))
(define (return-datum->type-expr terms)
    (match terms
        [[list type] (TypeAtom type)]
        [(cons type rest)
            (MapsTo 
                (TypeAtom type)
                (return-datum->type-expr rest))]
        [_ (error 'compiler-type/return "返回值不应为空。")])
)

(struct HeaderParts
    ([params : (Listof ValidYupianVarDeclar)]
    [return-terms : (Option (Listof Any))])
    #:transparent)

(: split-header-terms
    (-> (Listof Any) HeaderParts))

(define (split-header-terms terms)
    (let loop
        ([remaining terms]
        [params : (Listof ValidYupianVarDeclar) '()])
        (match remaining
        ['()
        (HeaderParts (reverse params) #f)]

        [(cons '-> return-terms)
        (if (null? return-terms)
            (error 'compiler-type/return "返回值须有一物在位。")
            (HeaderParts (reverse params) return-terms))]

        [(cons term rest)
        (if (valid-yupian-var-declar? term)
            (loop rest (cons term params))
            (error 'compiler-type/error
                    "函数参数声明无效: ~a"
                    term))])))

(: valid-yupian-fun-header-declar->middle (-> ValidYupianFunHeaderDeclar MiddleFunHeaderDeclar))
(define (valid-yupian-fun-header-declar->middle field)
    (define parts (split-header-terms (cdr field)))
    (define return-terms (HeaderParts-return-terms parts))

    (MiddleFunHeaderDeclar
        (car field) ; The name
        (map valid-yupian-var-declar->middle (HeaderParts-params parts))
        (if return-terms
            (ExplicitReturnType (return-datum->type-expr return-terms))
            (NoExplicitReturnType))))

(define-type ValidYupianFunHeaderDeclar
    (Pairof Symbol (Listof Any)))

(define-predicate valid-yupian-fun-header-declar? ValidYupianFunHeaderDeclar)

(struct MiddleVarDeclar
    [[name : Symbol] [type : Any]] #:transparent)

(: valid-yupian-var-declar->middle (-> ValidYupianVarDeclar MiddleVarDeclar))
(define (valid-yupian-var-declar->middle term)
    (match term
        [(list name _ type) (MiddleVarDeclar name type)]
        ; CAVEAT Default unwritten type to `Value` for the time being
        [name #:when (symbol? name) (MiddleVarDeclar name 'Value)]
        [_ (error 'compiler-internal-error "1")]))

(struct MiddleFunHeaderDeclar
    [
        [func-name : Symbol] 
        [params : (Listof MiddleVarDeclar)]
        [return-type : ReturnTypeMark]] #:transparent)
