#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     syntax/parse))

(provide (all-defined-out))

(begin-for-syntax
  (define (attach-name stx [base "g"])
    (define name (gensym base))
    (values (syntax-property stx 'syncheck:format:name name)
            name))
  )

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
     (with-syntax ([((expr ...) ...) exprss/name])
       (syntax-property
        (syntax/loc stx (cond [expr ...] ...))
        'syncheck:format
        (let ([body `#($$ ,@(for/list ([names (in-list namess)])
                              (define names-with-spaces (apply append (for/list ([name (in-list names)])
                                                                        (list " " name))))
                              `#(<> "[" #(options
                                          cond-body-line-break
                                          ,(cons 'preserve
                                                 `#(preserve-linebreak ,@names))
                                          ,(cons 'same-line
                                                 `#(<> ,@(if (pair? names-with-spaces)
                                                             (cdr names-with-spaces)
                                                             names-with-spaces)))
                                          ,(cons 'force-line-break
                                                 `#($$ ,@names)))
                                    "]")))])
          `#(<> "("
                #(options
                  cond-first-clause
                  ,(cons 'same-line
                         `#(<> ,(symbol->string (syntax-e #'form)) " " ,body))
                  ,(cons 'force-line-break
                         `#($$ ,(symbol->string (syntax-e #'form)) #(nest 1 ,body))))
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
