#lang racket/base

(require racket/port
         racket/runtime-path
         "../src/compiler.rkt")

(provide read-syntax get-info)

(define-runtime-path core-path "../main.rkt")

(define (read-syntax path port)
  (define forms
    (map s-tree->datum
         (parse-program (port->string port))))
  (datum->syntax
   #f
   `(module yupian typed/racket/base
      (require (file ,(path->string core-path)))
      ,@forms)))

(define (get-info key default)
  (case key
    [(color-lexer) #f]
    [else default]))