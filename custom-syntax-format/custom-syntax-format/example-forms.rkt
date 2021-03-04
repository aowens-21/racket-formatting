#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     syntax/parse
                     "combinator.rkt"))

(provide (all-defined-out))

(define-syntax (my-cond stx)
  (syntax-parse stx
    #:literals (else)
    [(form (expr:named ...+) ...)
     (define this-form (symbol->string (syntax-e #'form)))
     (define body
       (apply $$
              (for/list ([exprs (in-list (syntax-e #'((expr.stx ...) ...)))])
                (define exprs-with-spaces
                  (apply append (for/list ([expr (in-list (syntax-e exprs))])
                                  (list " " expr))))
                (<> "["
                    (options 'cond-body-line-break
                             'preserve         (apply preserve-linebreak (syntax-e exprs))
                             'same-line        (apply <>
                                                      (if (pair? exprs-with-spaces)
                                                          (cdr exprs-with-spaces)
                                                          exprs-with-spaces))
                             'force-line-break (apply $$ (syntax-e exprs)))
                    "]"))))
     (syntax-property
      (syntax/loc stx (cond [expr.stx ...] ...))
      'syncheck:format
      (<> "("
          (options 'cond-first-clause
                   'same-line        (<> this-form " " body)
                   'force-line-break ($$ this-form (nest 1 body)))
          ")"))]))

(define-syntax (my-let stx)
  (syntax-parse stx
    [(form ([lhs:named rhs:named] ...) body-expr:named ...+)
     (syntax-property
      (syntax/loc stx (let ([lhs.stx rhs.stx] ...) body-expr.stx ...))
      'syncheck:format
      (<> "("
          ($$ (<> (symbol->string (syntax-e #'form))
                  " ("
                  (apply $$
                         (for/list ([lhs (in-list (syntax-e #'(lhs.stx ...)))]
                                    [rhs (in-list (syntax-e #'(rhs.stx ...)))])
                           (<> "[" lhs " " rhs "]")))
                  ")")
              (nest 1
                    (apply $$ (syntax-e #'(body-expr.stx ...)))))
          ")"))]))
