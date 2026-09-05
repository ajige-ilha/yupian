#lang racket/base

(require racket/port
         racket/runtime-path
         "../src/compiler.rkt")

(provide read-syntax get-info)

(define-runtime-path core-path "../main.rkt")
(define-runtime-path stdlib-path "../src/stdlib.rkt")
(define-runtime-path compiler-path "../src/compiler.rkt")

(define (read-syntax path port)
  (define source
    (parse-program (port->string port)))
  (define forms
    (map s-tree->datum source))
  (datum->syntax
   #f
   `(module yupian typed/racket/base
      (require (file ,(path->string core-path))
               (file ,(path->string stdlib-path)))
      ,@forms)))

(define (get-info key default)
  (case key
    [(color-lexer) #f]
    [else default]))