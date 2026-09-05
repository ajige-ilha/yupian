#lang racket/base

(require rackunit
         racket/runtime-path
         "../src/compiler.rkt")

(define-runtime-path core-path "../main.rkt")

(check-equal?
 (map s-tree->datum (parse-program "「十 1 2」"))
 '((十 1 2)))

(check-equal?
 (map s-tree->datum (parse-program "『一 二』"))
 '((一 二)))

(define yupian-namespace (make-base-namespace))
(namespace-set-variable-value!
 '十
 (dynamic-require core-path '十)
 #t
 yupian-namespace)
(check-equal?
 (eval (s-tree->datum (car (parse-program "「十 2 3」")))
     yupian-namespace)
 5)

(check-equal?
 (map s-tree->datum
     (parse-program "(displayln \"「『not syntax』」\")"))
 '((displayln "「『not syntax』」")))

(check-equal?
 (map s-tree->datum
     (parse-program "(displayln \"escaped \\\"「still text」\")"))
 '((displayln "escaped \"「still text」")))

(displayln "compiler tests passed")