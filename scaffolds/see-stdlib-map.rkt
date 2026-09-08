#lang racket/base

(require racket/file string-tools)
(require "../src/compiler/compiler-to-typed-racket.rkt")

(define source
  (file->string "../src/stdlib/map.yupian"))

(define body
  (substring source
             (add1 (string-index source #\newline))))

(for ([tree (parse-program body)])
  (displayln (s-tree->datum tree)))