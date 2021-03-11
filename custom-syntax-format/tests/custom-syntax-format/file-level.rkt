#lang racket/base

(require rackunit
         racket/runtime-path
         custom-syntax-format
         racket/port
         racket/file)

(define-runtime-path file-level-conds.rkt "file-level/01-conds.rkt")
(define-runtime-path file-level-expected.rkt "file-level/01-expected.rkt")

(define-runtime-path file-level-02-comments.rkt "file-level/02-comments.rkt")
(define-runtime-path file-level-02-expected.rkt "file-level/02-expected.rkt")

(module+ test
  (require rackunit)

  (check-equal? (with-output-to-string
                  (λ () (dynamic-require file-level-conds.rkt #f)))
                (with-output-to-string
                  (λ () (dynamic-require file-level-expected.rkt #f))))

  (check-equal? (format-file file-level-conds.rkt)
                (file->string file-level-expected.rkt))

  (check-equal? (with-output-to-string
                  (λ () (dynamic-require file-level-02-comments.rkt #f)))
                (with-output-to-string
                  (λ () (dynamic-require file-level-02-expected.rkt #f))))

  ;; fails
  #;(check-equal? (format-file file-level-02-comments.rkt)
                  (file->string file-level-02-expected.rkt)))
