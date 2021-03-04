#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     syntax/parse
                     "combinator.rkt"))

(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class named
    (pattern _
             #:with fresh-name (datum->syntax #f (gensym "g"))
             #:attr name #'fresh-name
             #:attr stx (syntax-property this-syntax
                                         'syncheck:format:name
                                         (syntax-e #'fresh-name))))
  )

(define-syntax (my-cond stx)
  (syntax-parse stx
    #:literals (else)
    [(form (expr:named ...+) ...)
     (define this-form (symbol->string (syntax-e #'form)))
     (define body
       (apply $$
              (for/list ([names (in-list (syntax->datum #'((expr.name ...) ...)))])
                (define names-with-spaces
                  (apply append (for/list ([name (in-list names)])
                                  (list " " name))))
                (<> "[" (options
                         'cond-body-line-break
                         'preserve
                         (apply preserve-linebreak names)
                         'same-line
                         (apply <> (if (pair? names-with-spaces)
                                       (cdr names-with-spaces)
                                       names-with-spaces))
                         'force-line-break
                         (apply $$ names))
                    "]"))))
     (syntax-property
      (syntax/loc stx (cond [expr.stx ...] ...))
      'syncheck:format
      (<> "("
          (options
           'cond-first-clause
           'same-line (<> this-form " " body)
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
                         (for/list ([lhs-name (in-list (syntax->datum #'(lhs.name ...)))]
                                    [rhs-name (in-list (syntax->datum #'(rhs.name ...)))])
                           (<> "["
                               lhs-name
                               " "
                               rhs-name
                               "]")))
                  ")")
              (nest 1
                    (apply $$
                           (for/list ([body-name (in-list (syntax->datum #'(body-expr.name ...)))])
                             body-name))))
          ")"))]))
