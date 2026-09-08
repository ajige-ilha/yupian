#lang typed/racket/base

;;; Core values and operations for the Yupian Lisp DSL.

(provide
    OrdNum Value Real Integer Numeric
    木 水 火 土 竹 十 戈 大 中 一 弓 人 心 手 口 尸 廿 難 卜 卜耶
    generic-identity
    overloaded-add
    list-value stream-value reduce-value truthy? map-value math-value
    if-value dict-value match-value one-value struct-value new-value
    filter-value set-value loop-value false-value cons-value 
    lambda-value none-value)

(define-type Numeric Number) ; (U Real Integer))
(define-type OrdNum 
    (U
        Integer
        Real))
(define-type Value 
    (U 
        Number
        String
        Boolean
        Null
        (Listof Value)
        Procedure
    ))

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

(: 火 (All (A B) (-> (-> B A A) A (Listof B) A)))
(define (火 combine initial values)
    (foldl (lambda ([value : B] [result : A])
            (combine value result))
            initial
            values))

(: 土 True)
(define 土 #t)

(: 竹 (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (竹 transform values) (map transform values))

(: 十 (-> Numeric Numeric Numeric))
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

(define-syntax 口
    (syntax-rules (:)
        [(_ ugly-type condition ([name : type initial] ...) (step ...) final)
            (let: loop : ugly-type ([name : type (cast initial type)] ...)
                (if condition
                    (loop (cast step type) ...)
                    final))]
        [(_ condition ([name initial] ...) (step ...) final)
            (let loop ([name : Any initial] ...)
                (if condition
                    (loop step ...)
                    final))]))

(: 尸 False)
(define 尸 #f)

(: 廿 (-> Value (Listof Value) (Listof Value)))
(define (廿 value values) (cons value values))

(: 難 (All (A B) (-> (-> A B) (-> A B))))
(define (難 procedure) procedure)

(: 卜 (All (A) (-> (Listof A))))
(define (卜) '())

(: 卜耶 (-> Value Boolean))
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
(define-syntax loop-value
    (syntax-rules ()
        [(_ condition ([name initial] ...) (step ...) final)
         (口 condition ([name initial] ...) (step ...) final)]))
(define false-value 尸)
(define cons-value 廿)
(define lambda-value 難)
(define none-value (卜))