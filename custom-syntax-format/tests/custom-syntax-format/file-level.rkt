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
  (require syntax/modread
           racket/pretty)

  (define stx
    (call-with-input-file file-level-04-single-conds.rkt
      (λ (in)
        (port-count-lines! in)
        (with-module-reading-parameterization
          (λ () (read-syntax file-level-04-single-conds.rkt in))))))

  (define ns (make-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module (namespace-anchor->namespace
                              here-namespace-anchor)
                             ''#%builtin)
    (namespace-require '(only '#%kernel module module)))

  (define expanded-stx
    (parameterize ([current-namespace ns])
      (expand stx)))

  (define loc-info-map
    (build-loc-info-map expanded-stx))

  (port-count-lines! (current-output-port))
  (port-count-lines! (current-error-port))
  (pretty-write
   (parameterize ([current-output-port (open-output-nowhere)])
     (port-count-lines! (current-output-port))
     (print-file-with-format
      file-level-04-single-conds.rkt
      loc-info-map)))
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
