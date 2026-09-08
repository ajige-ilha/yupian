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
(define-type ValidYupianFunHeaderDeclar
    (Pairof Symbol (Listof ValidYupianVarDeclar)))

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
    [[func-name : Symbol] [params : (Listof MiddleVarDeclar)]] #:transparent)

(: valid-yupian-fun-header-declar->middle
    (-> ValidYupianFunHeaderDeclar MiddleFunHeaderDeclar))
(define (valid-yupian-fun-header-declar->middle field)
    (match field
        [(cons func-name vars) 
            (MiddleFunHeaderDeclar
                func-name
                (map valid-yupian-var-declar->middle vars))]
        [_ (error 'compiler-internal-error "2")]))
