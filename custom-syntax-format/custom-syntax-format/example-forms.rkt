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
         (<> "[" (preserve-linebreak expr1.stx expr2.stx ...) "]")
         ...)))
     (define this-form (symbol->string (syntax-e #'form)))
     (syntax-property
      (syntax/loc stx (cond [expr1.stx expr2.stx ...] ...))
      'syncheck:format
      (eval-format-template
       (quasiformat-template
        (<> "("
            (unformat this-form)
            " "
            (unformat body)
            ")"))))]))

(define-syntax (my-let stx)
  (syntax-parse stx
    [(form ([lhs:named rhs:named] ...) body-expr:named ...+)
     (syntax-property
      (syntax/loc stx (let ([lhs.stx rhs.stx] ...) body-expr.stx ...))
      'syncheck:format
      (eval-format-template
       (quasiformat-template
        (<> "("
            ($$ (<> (unformat (symbol->string (syntax-e #'form)))
                    " ("
                    ($$ (<> "[" lhs.stx " " rhs.stx "]") ...)
                    ")")
                (nest 1 ($$ body-expr.stx ...)))
            ")"))))]))

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
      (eval-format-template
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
                (unformat (attribute range.format)))
            ")"))))]))

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
      (eval-format-template
       (quasiformat-template
        (<> "("
            (unformat (symbol->string (syntax-e #'form)))
            " "
            (preserve-linebreak
             dom.stx ...
             (unformat (attribute range.format)))
            ")"))))]))
