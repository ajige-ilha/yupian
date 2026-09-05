#lang racket/base

(require rackunit
         racket/runtime-path
         "../../src/compiler.rkt"
         "../../src/stdlib.rkt")

(define-runtime-path core-path "../../main.rkt")
(define-runtime-path stdlib-path "../../src/stdlib.rkt")

(define yupian-namespace (make-base-namespace))
(parameterize ([current-namespace yupian-namespace])
    (namespace-require `(file ,(path->string core-path)))
    (namespace-require `(file ,(path->string stdlib-path))))

;;; Problem 1
(check-equal?
 (eval (s-tree->datum (car (parse-program "「梶 「木 一 二 四 三」」")))
     yupian-namespace)
 3)

(check-equal?
 (eval (s-tree->datum (car (parse-program "「梶 「木 一」」")))
     yupian-namespace)
 1)

(check-equal?
 (eval (s-tree->datum (car (parse-program "「梶 卜」")))
     yupian-namespace)
 '())
