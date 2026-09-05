#lang typed/racket/base

(require "../main.rkt")

(unless (= (assert (口 (< (assert n exact-integer?) 6)
              ([n 1] [sum 0])
              ((+ (assert n exact-integer?) 1)
               (+ (assert sum exact-integer?) (assert n exact-integer?)))
              (assert sum exact-integer?))
           exact-integer?)
           15)
    (error 'loop-test "口 returned the wrong result"))