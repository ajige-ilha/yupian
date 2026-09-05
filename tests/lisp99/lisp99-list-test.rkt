#lang racket/base

(require 
    rackunit
    racket/runtime-path
    "../../src/compiler.rkt"
    "../../src/stdlib.rkt"
    "../../src/lisp99/lisp99_ans.yupian")

(define-runtime-path core-path "../../main.rkt")
(define-runtime-path stdlib-path "../../src/stdlib.rkt")
(define-runtime-path path-99 "../../src/lisp99/lisp99_ans.yupian")

(define yupian-namespace (make-base-namespace))
(parameterize ([current-namespace yupian-namespace])
    (namespace-require `(file ,(path->string core-path)))
    (namespace-require `(file ,(path->string stdlib-path)))
    (namespace-require `(file ,(path->string path-99)))
    )

;;; Problem 1
(check-equal?
    (eval (s-tree->datum (car (parse-program "「朷 「木 一 二 四 三」」")))
        yupian-namespace)
    3)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「朷 「木 一」」")))
        yupian-namespace)
    1)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「朷 卜」")))
        yupian-namespace)
    '())


;;; Problem 2
(check-equal?
    (eval (s-tree->datum (car (parse-program "「答二 「木 一 二 四 三」」")))
        yupian-namespace)
    4)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「答二 「木 三 二」」")))
        yupian-namespace)
    3)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「答二 「木 一」」")))
        yupian-namespace)
    '())

(check-equal?
    (eval (s-tree->datum (car (parse-program "「答二 卜」")))
        yupian-namespace)
    '())

;;; Problem 3
(check-equal?
    (eval (s-tree->datum (car (parse-program "「柆 「木 一 二 四 三」 零」")))
        yupian-namespace)
    1)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「柆 「木 一 二 四 三」 二」")))
        yupian-namespace)
    4)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「柆 「木 一 二 四 三」 五」")))
        yupian-namespace)
    '())

