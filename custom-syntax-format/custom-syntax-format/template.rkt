#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/sequence
         "combinator.rkt")

(provide (all-defined-out)
         (rename-out [unsyntax unformat]))

(define (format-datum? v)
  (or (symbol? v)
      (string? v)))

(define (eval-format-datum v)
  (match v
    [(? symbol? name)
     name]
    [(? string? strlit)
     strlit]))

(define (eval-format-template stx)
  (cond
    [(syntax-property stx 'syncheck:format:name)
     (source stx (syntax-property stx 'syncheck:format:name))]
    [else
     (match (syntax-e stx)
       ;; datum
       [(? format-datum? v)
        (eval-format-datum v)]
       [(list (? identifier? form) stx-arg)
        #:when (free-identifier=? form #'quote)
        (eval-format-datum (syntax-e stx-arg))]
       ;; combinators
       [(cons (? identifier? op) stx-args)
        #:when (free-identifier=? op #'<>)
        (apply <> (map eval-format-template stx-args))]
       [(cons (? identifier? op) stx-args)
        #:when (free-identifier=? op #'$$)
        (apply $$ (map eval-format-template stx-args))]
       [(cons (? identifier? op) stx-args)
        #:when (free-identifier=? op #'preserve-linebreak)
        (apply preserve-linebreak (map eval-format-template stx-args))]
       [(list (? identifier? op) stx-depth stx-arg)
        #:when (free-identifier=? op #'nest)
        (define depth (syntax-e stx-depth))
        (nest depth (eval-format-template stx-arg))]
       [(list (? identifier? op) stx-source)
        #:when (free-identifier=? op #'source)
        (source stx-source)]
       [(cons (? identifier? op) (cons stx-name stx-options))
        #:when (free-identifier=? op #'options)
        (define name (syntax-e stx-name))
        (apply options
               name
               (apply append
                      (for/list ([stx-an-option (in-slice 2 (in-list stx-options))])
                        (list (syntax-e (list-ref stx-an-option 0))
                              (eval-format-template (list-ref stx-an-option 1))))))])]))

(define-syntax (quasiformat-template stx)
  (syntax-parse stx
    [(_ format:expr)
     (syntax/loc stx
       (quasisyntax format))]))

(module+ test
  (require (submod "..")
           rackunit)

  (check-equal?
   (eval-format-template
    (quasiformat-template
     (<> name1 "literal2"
         ($$ "[]"
             (preserve-linebreak "[]" 'answer)))))
   (<> 'name1 "literal2"
       ($$ "[]"
           (preserve-linebreak "[]" 'answer))))

  (check-equal?
   (eval-format-template
    (quasiformat-template
     (nest 5 "okay")))
   (nest 5 "okay"))

  (check-equal?
   (eval-format-template
    (quasiformat-template
     (options config-name opt1 "A" opt2 name opt3 ($$ "okay"))))
   (options 'config-name
            'opt1 "A"
            'opt2 'name
            'opt3 ($$ "okay")))

  (check-equal?
   (eval-format-template
    (quasiformat-template
     (unformat (string-append "hello " "world"))))
   "hello world")

  (check-equal?
   (eval-format-template
    (quasiformat-template
     (<> (unformat
          (quasiformat-template
           (nest 5 "okay"))))))
   (<> (nest 5 "okay")))
  )
