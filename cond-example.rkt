#lang racket/base

(require racket/match
         racket/file
         (for-syntax racket/base
                     racket/sequence
                     syntax/parse))

(begin-for-syntax
  (define (attach-name stx [base "g"])
    (define name (gensym base))
    (values (syntax-property stx 'syncheck:format:name name)
            name))
  )

(port-count-lines! (current-output-port))

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
              ;; TODO: Think about a construct for spacing/indenting between elements,
              ;; instead of this explicit space
              " "
              #($$ ,@(for/list ([names (in-list namess)])
                       `#(<> "[" #(preserve-linebreak ,@names) "]")))
              ")")))]))

#|
    |"(" "let" " (" |"[" a 10 ... "]"
                    |"[" ..... "]" ")"

    |__ body.... ")"
|#


#|
    "("|"let" " (" |"[" a 10 ... "]"
                   |"[" ..... "]" ")"
       |_body....         ")"
|#

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

#|

What are our goals (for now)?

1. Check if our combinator "language" can express my-cond and my-let
  formatting rules

2. Define what each combinator means

|#


#|

Current Formatting Language

string
    prints the literal string
#(source source-file line column position span)
    looks up the source location in the file and prints it (starting at
    source location and using span)
#(<> element ...)
    prints each element consecutively (not smart, will probably break
    for multi-line elements)
#($$ element ...)
    inserts each element with a linebreak in between, and each element
    starts at the same column
#(preserve-linebreak element ...)
    uses existing line break information to decide whether to print one
    line between elements or just a space
#(nest int element)
    increase the nesting depth by int columns

TODOS:
- Come up with a combinator that handles nesting
- Also come up with a combinator that decides whether or not to break lines between elements
- The comment information is dropped entirely

|#
     
(define (extract-name-syntax-maps stx)
  (define table (make-hash))
  (define (do-traverse stx)
    (define name (syntax-property stx 'syncheck:format:name))
    (when name
      (hash-set! table name stx))
    (define datum (syntax-e stx))
    (cond
      [(list? datum)
       (for ([substx (in-list datum)])
         (do-traverse substx))]
      [(vector? datum)
       (for ([substx (in-vector datum)])
         (do-traverse substx))]
      [(pair? datum)
       (do-traverse (car datum))
       (do-traverse (cdr datum))]
      ;; TODO recursively traverse all compound data
      [else (void)]))

  (do-traverse stx)
  table)

(define (construct-pretty-print-info table pp-info)
  (match pp-info
    [(? string? s) s]
    [(? symbol? name)
     (define stx/#f
       (hash-ref table name (λ () #f)))
     (cond
       [(not stx/#f)
        (format "MISSING:~a" name)]
       [else
        (construct-pretty-print-info-from-syntax
         table
         stx/#f)])]
    [`#(<> ,elements ...)
     `#(<>
        ,@(for/list ([element (in-list elements)])
            (construct-pretty-print-info table element)))]
    [`#($$ ,elements ...)
     `#($$
        ,@(for/list ([element (in-list elements)])
            (construct-pretty-print-info table element)))]
    [`#(preserve-linebreak ,elements ...)
     `#(preserve-linebreak
        ,@(for/list ([element (in-list elements)])
            (construct-pretty-print-info table element)))]
    [`#(nest ,depth ,element)
     `#(nest ,depth
             ,(construct-pretty-print-info table element))]))

(define (construct-pretty-print-info-from-syntax table stx)
  (define pp-info
    (syntax-property stx 'syncheck:format))
  (cond
    [(not pp-info)
     `#(source ,(syntax-source stx)
               ,(syntax-line stx)
               ,(syntax-column stx)
               ,(syntax-position stx)
               ,(syntax-span stx))]
    [else
     (construct-pretty-print-info table pp-info)]))

(define pretty-print-indentation (make-parameter 0))

(define (pretty-print-not-really-newline)
  (newline)
  (for ([i (in-range (pretty-print-indentation))])
    (write-char #\space)))

(define (pretty-print-not-really pp-info)
  (match pp-info
    [(? string? s)
     (write-string s)]
    [`#(source ,source ,line ,col ,pos ,span)
     (write-string
      (substring (file->string source)
                 (sub1 pos)
                 (sub1 (+ pos span))))]
    [`#(<> ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (pretty-print-not-really element))]
    [`#($$ ,element0 ,elements ...)
     (indent-at-current-col
      (lambda ()
        (pretty-print-not-really element0)
        (for ([(element idx) (in-indexed elements)])
          (pretty-print-not-really-newline)
          (pretty-print-not-really element))))]
    [`#($$)
     (void)]
    [`#(preserve-linebreak
        ,(and element0 `#(source ,source0 ,line0 ,col0 ,pos0 ,span0))
        ,(and elements `#(source ,source ,line ,col ,pos ,span)) ...)
     (indent-at-current-col
      (lambda ()
        (pretty-print-not-really element0)
        (for ([previous-line-number (in-list (cons line0 line))]
              [current-line-number (in-list line)]
              [element (in-list elements)])
          (cond
            [(> current-line-number previous-line-number)
             (pretty-print-not-really-newline)]
            [else
             (write-char #\space)])
          (pretty-print-not-really element))))]
    [`#(preserve-linebreak ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (write-char #\space))
       (pretty-print-not-really element))]
    [`#(nest ,(? exact-integer? depth) ,element)
     (for ([i (in-range depth)])
       (write-char #\space))
     (indent-at-current-col
      (lambda ()
        (pretty-print-not-really element)))]))

(define (indent-at-current-col proc)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  (parameterize ((pretty-print-indentation col))
    (proc)))

(define (racket-format stx)
  (define expanded-stx (expand stx))
  (pretty-print-not-really
   (construct-pretty-print-info-from-syntax (extract-name-syntax-maps expanded-stx)
                                            expanded-stx)))
#|
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
|#

(define my-cond-stx-1
   #'(my-cond (#f "false") [(< 10 5)
                            "a"] (#t "b") (else
                                                    #f)
              ))

(racket-format my-cond-stx-1)
(newline)

#|
(expand
 #'(my-cond ((my-cond ((not #f) 'is-true)))
            (else
             #f)))
|#

(define my-cond-stx-2
   #'(my-cond ((my-cond ((not #f)
                         'is-true)))
              (else
             #f)))

(racket-format my-cond-stx-2)
(newline)


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

#|

(my-let ([a 10] [b 5] [c
                       20])
   (+ a b
        c))

--->

(my-let ([a 10]
         [b 5]
         [c 20])
   (+ a b
        c))

|#

(define my-let-stx-1
  #'(my-let ([a 10] [b 5] [c
                       20])
         (+ a b
              c)))

(racket-format my-let-stx-1)
(newline)

(define my-let-stx-2
  #'(my-let ([a 10] [b 5] [c
                       (* 7
                          3)])
         (+ a b
              c)))

(racket-format my-let-stx-2)
(newline)
