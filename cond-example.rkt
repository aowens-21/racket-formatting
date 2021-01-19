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
            (construct-pretty-print-info table element)))]))

(define (construct-pretty-print-info-from-syntax table stx)
  (define pp-info
    (syntax-property stx 'syncheck:format))
  (cond
    [(not pp-info)
     `#(source ,(syntax-source stx)
               ,(syntax-line stx)
               ,(syntax-column stx)
               ,(syntax-span stx)
               ,(syntax-position stx))]
    [else
     (construct-pretty-print-info table pp-info)]))

(define (pretty-print-not-really-newline)
  (newline))

(define (pretty-print-not-really pp-info)
  (match pp-info
    [(? string? s)
     (write-string s)]
    [`#(source ,source ,line ,col ,span ,pos)
     (write-string
      (substring (file->string source)
                 (sub1 pos)
                 (sub1 (+ pos span))))]
    [`#(<> ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (write-char #\space))
       (pretty-print-not-really element))]
    [`#($$ ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (pretty-print-not-really-newline))
       (pretty-print-not-really element))]
    [`#(preserve-linebreak
        ,(and element0 `#(source ,source0 ,line0 ,col0 ,span0 ,pos0))
        ,(and elements `#(source ,source ,line ,col ,span ,pos)) ...)
     (pretty-print-not-really element0)
     (for ([previous-line-number (in-list (cons line0 line))]
           [current-line-number (in-list line)]
           [element (in-list elements)])
       (for ([i (in-range (- current-line-number previous-line-number))])
         (pretty-print-not-really-newline))
       (pretty-print-not-really element))]
    [`#(preserve-linebreak ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (write-char #\space))
       (pretty-print-not-really element))]))

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
  (expand
   #'(my-cond (#f "false") [(< 10 5) "a"] (#t "b") (else
                                                    #f)
              )))

(define tbl-1
  (extract-name-syntax-maps my-cond-stx-1))

(pretty-print-not-really
(construct-pretty-print-info-from-syntax tbl-1
                                         my-cond-stx-1))

#|
(expand
 #'(my-cond ((my-cond ((not #f) 'is-true)))
            (else
             #f)))
|#

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
