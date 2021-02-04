# Racket Formatting

## Memo
### What are our goals (for now)?

1. Figure out how to make combinators customizable for the user of the formatting tool,
   types of customization configured by the macro authors.

    1. Customizable combinators should provide some information for forms that don't have
       a pretty printing specification (for example, DrRacket's default indentation rules)

2. Abstract over the naming pattern:

    ```racket
    (syntax-parse stx
      #:literals (else)
      [(form (expr ...+) ...)
       (define-values (exprss/name namess)
         (for/lists (exprss/name namess)
                    ([exprs (in-syntax #'((expr ...) ...))])
           (for/lists (exprs/name names)
                      ([expr (in-syntax exprs)])
             (attach-name expr "my-cond.clause"))))
    ```

### TODOS:

- Add a test case for pretty-printing the whole file.
    * What (top-level?) API should provided?

- Think: racket-format currently includes syntax information from `'disappeared-use`.
  Do we want to ignore `'disappeared-use` and have an optional opt-in instruction in the
  formatting language instead?
    * The macro authors should correctly produce pretty printing annotations,
      including identifiers that will disappear (e.g. `else` in `cond`).
      But perhaps manually producing that is inconvenient.
- Also come up with a combinator that decides whether or not to break lines between elements
- The comment information is dropped entirely

## Current Formatting Language

- `string`

    prints the literal string

- `#(source source-file line column position span)`

    looks up the source location in the file and prints it (starting at
    source location and using span)

- `#(<> element ...)`

    prints each element consecutively (not smart, will probably break
    for multi-line elements)

- `#($$ element ...)`

    inserts each element with a linebreak in between, and each element
    starts at the same column

- `#(preserve-linebreak element ...)`

    uses existing line break information to decide whether to print one
    line between elements or just a space

- `#(nest int element)`

    increase the nesting depth by int columns

## Misc old comments

cond drops syntax properties on each clauses

```racket
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

```

others

```racket
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
```

DONE - Shift the indentation of the `#(source ...)` block

    ```racket
             [a]bcdef
               ghijk
           lmnop
    
       |#(source ...)
    =>
       |[a]bcdef
       |  ghijk
      lmnop
    ```
## Finished Tasks

1. Check if our combinator "language" can express my-cond and my-let
    formatting rules

2. Define what each combinator means
