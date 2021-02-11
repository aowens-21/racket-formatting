#lang racket/base

(require rackunit
         racket/runtime-path
         custom-syntax-format)

(define-runtime-path file-level-conds.rkt "file-level-conds.rkt")
(format-file file-level-conds.rkt)
