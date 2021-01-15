#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     syntax/parse))

(begin-for-syntax
  (define (attach-name stx [base "g"])
    (define name (gensym base))
    (values (syntax-property stx 'syncheck:format:name name)
            name))
  )

;; FIXME handle `else` and properly format it because
;; it is moved into the 'disappeared-use property
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
        `#(<> "("
              ,(symbol->string (syntax-e #'form))
              #($$ ,@(for/list ([names (in-list namess)])
                       `#(<> "[" #(preserve-linebreak ,@names) "]")))
              ")")))]))

(expand
 #`(cond
     ;; the syntax property attached to the clauses will be dropped by cond
     #,(syntax-property #`['no #,(syntax-property #''get
                                                  'GET
                                                  "HERE")]
                        'clause
                        "This syntax property is gone")
     [else #,(syntax-property #''okay
                              'else
                              "OKAY")]))

(expand
 #'(my-cond (#f "false") [(< 10 5) "a"] (#t "b") (else
                                                  #f)
            ))

(expand
 #'(my-cond ((my-cond ((not #f) 'is-true)))
            (else
             #f)))

#;
'(
  ;; pretty-formatting of cond forms
  (my-cond (#f "false") [(< 10 5) "a"] (#t "b") (else
                                                 #f)
           )
  ;=>
  (my-cond [#f "false"]
           [(< 10 5) "a"]
           [#t "b"]
           [else
            #f])



  ;; recursive call of pretty-print-doc, a hypothetical function for
  ;; computing the data structure representation of pretty-printing
  ;; information.
  (pretty-print-doc
   (my-cond (#f "false") [(< 10 5) "a"] (#t "b") (else
                                                  #f)
            ))
  ;=>
  #(<>
    "(my-cond"
    #($$ #(<> "[" 'clause1.Q 'clause1.A "]")
         #(<> "[" 'clause2.Q 'clause2.A "]")
         #(<> "[" 'clause3.Q 'clause3.A "]"))
    ")")
  ;; where
  'clause1.Q #f
  'clause1.A "false"
  etc.

  ;; and
  (pretty-print-doc #f)
  ;; =>
  'clause1.Q

  ;; and
  (pretty-print-doc "false")
  ;; =>
  'clause1.A
  )
