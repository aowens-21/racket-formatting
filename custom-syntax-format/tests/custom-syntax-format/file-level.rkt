#lang racket/base

(require rackunit
         racket/runtime-path
         custom-syntax-format
         racket/file)

(define-runtime-path file-level-conds.rkt "file-level-conds.rkt")
(define-runtime-path file-level-expected.rkt "file-level-expected.rkt")

(module+ test
  (require rackunit)

  (check-equal? (format-file file-level-conds.rkt)
                (file->string file-level-expected.rkt)))
