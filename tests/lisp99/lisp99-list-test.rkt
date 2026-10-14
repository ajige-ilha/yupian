#lang racket/base

(require 
    rackunit
    racket/runtime-path
    "../../src/compiler/mod.rkt"
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
    (eval (s-tree->datum (car (parse-program "「朷 「卜」」")))
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
    (eval (s-tree->datum (car (parse-program "「答二 「卜」」")))
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

;;; Problem 4
;;; FIXME Remove references to #'十木 when own type system works
(check-equal?
    (eval (s-tree->datum (car (parse-program "「十木 「木 一 二 四 三」 」")))
        yupian-namespace)
    4)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「十木 「木 一」 」")))
        yupian-namespace)
    1)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「十木 「卜」」")))
        yupian-namespace)
    0)

;;; Problem 5
(check-equal?
    (eval (s-tree->datum (car (parse-program "「桝 「木 一 二 四 三」 」")))
        yupian-namespace)
    '(3 4 2 1))

;;; Problem 6
(check-equal?
    (eval (s-tree->datum (car (parse-program "「答六 「木 一 二 四 三」 」")))
        yupian-namespace)
    #f)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「答六 「木 一 二 三 二 一」 」")))
        yupian-namespace)
    #t)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「答六 「木 一」 」")))
        yupian-namespace)
    #t)

(check-equal?
    (eval (s-tree->datum (car (parse-program "「答六 「卜」 」")))
        yupian-namespace)
    #t)

;;; Problem 7
(check-equal?
    (eval (s-tree->datum (car (parse-program "「枰 「木 一 「木 二 「木 三 四」 五」」 」")))
        yupian-namespace)
    (list 1 2 3 4 5))

;;; Problem 8
(check-equal?
    (eval (s-tree->datum (car (parse-program "「橉 「木 一 一 一 一 二 三 三 一 一 四 五 五 五 五 」 」")))
        yupian-namespace)
    (list 1 2 3 1 4 5))

;;; Problem 9
(check-equal?
    (eval (s-tree->datum (car (parse-program "「答九 「木 一 一 一 一 二 三 三 一 一 四 五 五 五 五 」 」")))
        yupian-namespace)
    (list (list 1 1 1 1) (list 2) (list 3 3) (list 1 1) (list 4) (list 5 5 5 5)))

;;; Problem 10
#|
(check-equal?
    (eval (s-tree->datum (car (parse-program "「楄 「木 '天 '天 '天 '天 '地 '玄 '玄 '天 '天 '黄 '宇 '宇 '宇 '宇 」 」")))
        yupian-namespace)
    '((4 天) (1 地) (2 玄) (2 天) (1 黄) (4 宇)))
|#
