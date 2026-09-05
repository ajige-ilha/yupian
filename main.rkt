#lang typed/racket/base

;;; Core values and operations for the Yupian Lisp DSL.

(provide
    Value Real Integer Numeric
    木 水 火 土 竹 十 戈 大 中 一 弓 人 心 手 口 尸 廿 山 田 難 卜 卜耶
    generic-identity
    overloaded-add
    list-value stream-value reduce-value truthy? map-value math-value
    if-value dict-value match-value one-value struct-value new-value
    filter-value set-value loop-value false-value cons-value define-value
    let-value lambda-value none-value)

(define-type Value Any)
(define-type Numeric (U Real Integer))

(: generic-identity (All (A) (-> A A)))
(define (generic-identity value) value)

(: overloaded-add
     (case-> (-> Integer Integer)
                     (-> Integer Integer Integer)))
(define overloaded-add
    (case-lambda
        [(value) value]
        [(left right) (+ left right)]))

(: 木 (-> Value * (Listof Value)))
(define (木 . values) values)

(: 水 (-> (Listof Value) (Sequenceof Value)))
(define (水 values) (in-list values))

(: 火 (-> (-> Value Value Value) Value (Listof Value) Value))
(define (火 combine initial values)
    (foldl (lambda ([value : Value] [result : Value])
            (combine value result))
            initial
            values))

(: 土 (-> Any Boolean))
(define (土 value) (not (or (eq? value #f) (void? value))))

(: 竹 (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (竹 transform values) (map transform values))

(: 十 (-> Number Number Number))
(define (十 left right) (+ left right))

(define-syntax 戈
        (syntax-rules ()
                [(_ condition when-true when-false)
                 (if condition when-true when-false)]))

(: 大 (-> (Listof (Pairof Symbol Value)) (Immutable-HashTable Symbol Value)))
(define (大 entries) (make-immutable-hash entries))

(define-syntax 中
    (syntax-rules ()
        [(_ clause ...)
         (cond clause ...)]))

(: 一 Integer)
(define 一 1)

(: 弓 (-> (Listof Symbol) (Listof Value) (Immutable-HashTable Symbol Value)))
(define (弓 names values)
    (make-immutable-hash
    (map (lambda ([name : Symbol] [value : Value]) (cons name value))
        names
        values)))

(: 人 (All (A) (-> A A)))
(define (人 value) value)

(: 心 (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (心 keep? values) (filter keep? values))

(: 手 (All (A) (-> (Boxof A) A Void)))
(define (手 target value)
    (set-box! target value))

(: 口 (-> Natural (Listof Natural)))
(define (口 count)
    (build-list count (lambda ([index : Index]) index)))

(: 尸 False)
(define 尸 #f)

(: 廿 (All (A B) (-> A (Listof B) (Listof (U A B)))))
(define (廿 value values) (cons value values))

(define-syntax 山
    (syntax-rules ()
        [(_ (function-name argument ...) body)
         (define (function-name argument ...) body)]
        [(_ name value)
         (define name value)]))

(define-syntax 田
    (syntax-rules ()
        [(_ function-name (argument ...) body)
         (define (function-name argument ...) body)]
        [(_ [name value] body)
         (let* ([name value]) body)]
        [(_ ([name value] ...) body)
         (let* ([name value] ...) body)]))

(: 難 (All (A B) (-> (-> A B) (-> A B))))
(define (難 procedure) procedure)

(: 卜 Null)
(define 卜 '())

(: 卜耶 (-> Any Boolean))
(define (卜耶 value) (null? value))

(define list-value 木)
(define stream-value 水)
(define reduce-value 火)
(define truthy? 土)
(define map-value 竹)
(define math-value 十)
(define-syntax if-value
    (syntax-rules ()
        [(_ condition when-true when-false)
         (戈 condition when-true when-false)]))
(define dict-value 大)
(define-syntax match-value
    (syntax-rules ()
        [(_ clause ...)
         (中 clause ...)]))
(define one-value 一)
(define struct-value 弓)
(define new-value 人)
(define filter-value 心)
(define set-value 手)
(define loop-value 口)
(define false-value 尸)
(define cons-value 廿)
(define-syntax define-value
    (syntax-rules ()
        [(_ form ...)
         (山 form ...)]))
(define-syntax let-value
    (syntax-rules ()
        [(_ form ...)
         (田 form ...)]))
(define lambda-value 難)
(define none-value 卜)