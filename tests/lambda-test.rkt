#lang typed/racket/base

(require "../main.rkt")

(assert (= ((難 (x) (+ x 2)) 3) 5))
