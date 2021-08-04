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

(define-runtime-path file-level-03-nested-contracts.rkt "file-level/03-nested-contracts.rkt")
(define-runtime-path file-level-03-expected.rkt "file-level/03-expected.rkt")

(define-runtime-path file-level-04-single-conds.rkt "file-level/99-conds.rkt")
(define-runtime-path file-level-04-expected.rkt "file-level/99-expected.rkt")

(module+ main
  (require racket/pretty)

  (pretty-write
   (get-format-instructions file-level-04-single-conds.rkt))
  )

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

  (check-equal? (with-output-to-string
                  (λ () (dynamic-require file-level-03-nested-contracts.rkt #f)))
                (with-output-to-string
                  (λ () (dynamic-require file-level-03-expected.rkt #f))))

  (check-equal? (format-file file-level-03-nested-contracts.rkt)
                (file->string file-level-03-expected.rkt))

  (check-equal? (with-output-to-string
                  (λ () (dynamic-require file-level-04-single-conds.rkt #f)))
                (with-output-to-string
                  (λ () (dynamic-require file-level-04-expected.rkt #f))))

  (check-equal? (format-file file-level-04-single-conds.rkt)
                (file->string file-level-04-expected.rkt))

  ;; fails
  #;(check-equal? (format-file file-level-02-comments.rkt)
                  (file->string file-level-02-expected.rkt)))
