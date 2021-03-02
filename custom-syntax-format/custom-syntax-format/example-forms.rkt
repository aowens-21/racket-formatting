#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     syntax/parse
                     "combinator.rkt"))

(provide (all-defined-out))

(define-syntax (my-cond stx)
  (syntax-parse stx
    #:literals (else)
    [(form (expr ...+) ...)
     (define-values (exprss/name namess)
       (for/lists (exprss/name namess)
                  ([exprs (in-syntax #'((expr ...) ...))])
         (for/lists (exprs/name names)
                    ([expr (in-syntax exprs)])
           (attach-name expr "my-cond.clause"))))
     (define this-form (symbol->string (syntax-e #'form)))
     (with-syntax ([((expr ...) ...) exprss/name])
       (syntax-property
        (syntax/loc stx (cond [expr ...] ...))
        'syncheck:format
        (let ([body (apply $$
                           (for/list ([names (in-list namess)])
                             (define names-with-spaces (apply append (for/list ([name (in-list names)])
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
                                 "]")))])
          (<> "("
              (options
               'cond-first-clause
               'same-line (<> this-form " " body)
               'force-line-break ($$ this-form (nest 1 body)))
              ")"))))]))

(define-syntax (my-let stx)
  (syntax-parse stx
    [(form ([lhs rhs] ...) body-expr ...+)
     (define-values (exprss/name namess)
       (for/lists (exprss/name namess)
                  ([exprs (in-syntax #'((lhs rhs) ...))])
         (for/lists (exprs/name names)
                    ([expr (in-syntax exprs)])
           (attach-name expr "my-let.clause"))))
     (define-values (body-exprss/name body-namess)
       (for/lists (body-exprss/name body-namess)
                  ([body (in-syntax #'(body-expr ...))])
         (attach-name body "my-let-body.clause")))
     (with-syntax ([((lhs rhs) ...) exprss/name]
                   [(body-expr ...) body-exprss/name])
       (syntax-property
        (syntax/loc stx (let ([lhs rhs] ...) body-expr ...))
        'syncheck:format
        `#(<> "("
              #($$ #(<> ,(symbol->string (syntax-e #'form))
                        " ("
                        #($$ ,@(for/list ([names (in-list namess)])
                                 `#(<> "["
                                       ,(list-ref names 0)
                                       " "
                                       ,(list-ref names 1)
                                       "]")))
                        ")")
                   #(nest 1
                          #($$ ,@(for/list ([body-name (in-list body-namess)])
                                   body-name))))
              ")")))]))
