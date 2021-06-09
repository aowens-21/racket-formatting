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

(begin-for-syntax
  (define-syntax-class arrow-contract-range
    #:literals (values any)
    #:attributes (stx format)
    #:commit
    (pattern ((~and values-literal values) values-range-ctc:named ...)
             #:attr stx
             (syntax/loc this-syntax
               (values-literal values-range-ctc.stx ...))
             #:attr format
             (quasiformat-template
              (<> "("
                  (unformat (symbol->string (syntax-e #'values-literal)))
                  " "
                  ($$ values-range-ctc.stx ...)
                  ")")))
    (pattern (~and any-literal:named any)
             #:attr stx    #'any-literal.stx
             #:attr format (quasiformat-template
                            any-literal.stx))
    (pattern range-ctc:named
             #:attr stx    #'range-ctc.stx
             #:attr format (quasiformat-template
                            range-ctc.stx)))
  )

(define-syntax (my:->* stx)
  (syntax-parse stx
    [(form (mandatory-dom:named ...)
           (~optional (optional-dom:named ...))
           range:arrow-contract-range)
     (define expanded->*
       (local-expand (quasisyntax/loc stx
                       (->* (mandatory-dom.stx ...)
                            (~? (optional-dom.stx ...))
                            range.stx))
                     (syntax-local-context)
                     #f))
     (syntax-property
      (datum->syntax expanded->* (syntax-e expanded->*) stx expanded->*)
      'syncheck:format
      (quasiformat-template
       (<> "("
           (unformat (symbol->string (syntax-e #'form)))
           " "
           ($$ (<> "("
                   ($$ mandatory-dom.stx ...)
                   ")")
               (~?
                (<> "("
                    ($$ optional-dom.stx ...)
                    ")"))
               (format-embed (unformat (attribute range.format))))
           ")")))]))

(define-syntax (my:-> stx)
  (syntax-parse stx
    #:literals (values)
    [(form dom:named ... range:arrow-contract-range)
     (define expanded->*
       (local-expand (quasisyntax/loc stx
                       (-> dom.stx ... range.stx))
                     (syntax-local-context)
                     #f))
     (syntax-property
      (datum->syntax expanded->* (syntax-e expanded->*) stx expanded->*)
      'syncheck:format
      (quasiformat-template
       (<> "("
           (unformat (symbol->string (syntax-e #'form)))
           " "
           (preserve-linebreak
            dom.stx ...
            (format-embed (unformat (attribute range.format))))
           ")")))]))
