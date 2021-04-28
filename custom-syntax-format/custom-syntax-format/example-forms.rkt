#lang racket/base

(require racket/contract)

(require (for-syntax racket/base
                     racket/sequence
                     syntax/parse
                     "combinator.rkt"
                     "template.rkt"))

(provide (all-defined-out))

(define-syntax (my-cond stx)
  (syntax-parse stx
    #:literals (else)
    [(form (expr1:named expr2:named ...) ...)
     (define body
       (quasiformat-template
        ($$
         (<> "[" (options cond-body-line-break
                          preserve         (preserve-linebreak expr1.stx expr2.stx ...)
                          same-line        (<> expr1.stx (~@ " " expr2.stx) ...)
                          force-line-break ($$ expr1.stx expr2.stx ...))
             "]")
         ...)))
     (define this-form (symbol->string (syntax-e #'form)))
     (syntax-property
      (syntax/loc stx (cond [expr1.stx expr2.stx ...] ...))
      'syncheck:format
      (quasiformat-template
       (<> "("
           (options cond-first-clause
                    same-line        (<> (unformat this-form) " "
                                         (format-embed (unformat body)))
                    force-line-break ($$ (unformat this-form)
                                         (nest 1 (format-embed (unformat body)))))
           ")")))]))

(define-syntax (my-let stx)
  (syntax-parse stx
    [(form ([lhs:named rhs:named] ...) body-expr:named ...+)
     (syntax-property
      (syntax/loc stx (let ([lhs.stx rhs.stx] ...) body-expr.stx ...))
      'syncheck:format
      (quasiformat-template
       (<> "("
           ($$ (<> (unformat (symbol->string (syntax-e #'form)))
                   " ("
                   ($$ (<> "[" lhs.stx " " rhs.stx "]") ...)
                   ")")
               (nest 1 ($$ body-expr.stx ...)))
           ")")))]))

(define-syntax (my:->* stx)
  (syntax-parse stx
    #:literals (values)
    [(form (mandatory-dom:named ...) ((~and values-lit values) range:named ...))
     (syntax-property
      (syntax/loc stx (->* (mandatory-dom ...) (values range ...)))
      'syncheck:format
      (quasiformat-template
       (<> "("
           (unformat (symbol->string (syntax-e #'form)))
           " "
           ($$ (<> "("
                   ($$ (source mandatory-dom.stx) ...)
                   ")")
               (<> "("
                   (unformat (symbol->string (syntax-e #'values-lit)))
                   " "
                   ($$ (source range.stx) ...)
                   ")"))
           ")")))]
    [(form (mandatory-dom:named ...) range:named)
     (syntax-property
      (syntax/loc stx (->* (mandatory-dom ...) range))
      'syncheck:format
      (quasiformat-template
       (<> "("
           (unformat (symbol->string (syntax-e #'form)))
           " "
           ($$ (<> "("
                   ($$ (source mandatory-dom.stx) ...)
                   ")")
               (source range.stx))
           ")")))]))
