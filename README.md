# Racket Formatting

## Memo
### Terminology

- the _programmers_ : the person implementing macros
- the _user(s)_ : the person using the macros and the formatting tool

### What are our goals (for now)?

1. Abstract over the naming pattern:

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

2. Customizable combinators should provide some information for forms that don't have
   a pretty printing specification (for example, DrRacket's default indentation rules)

### TODOS:

- Think: racket-format currently includes syntax information from `'disappeared-use`.
  Do we want to ignore `'disappeared-use` and have an optional opt-in instruction in the
  formatting language instead?
    * The macro authors should correctly produce pretty printing annotations,
      including identifiers that will disappear (e.g. `else` in `cond`).
      But perhaps manually producing that is inconvenient.
- Also come up with a combinator that decides whether or not to break lines between elements

### Possible Future Works

- The comment information is dropped entirely
- Handle reading-time indentation, e.g. `@`-expressions. Currently DrRacket's indentation
  engine does not work in the following case.

    ```racket
    (match result
      [('check-satisfied (list actual))
       @list{
         Test did not pass:
           @'(embed-pretty-print-no-eol/image actual)
           did not satisfy the check
             @'(embed-pretty-write-code (third complete-test))
           in the test case
             @'(embed-pretty-write-code complete-test)

       }])])
    ```

## Current Formatting Language

Formatting programs are written using the `element` grammar.
An `element` is one of:

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

- `#(options name-symbol (option-symbol . element) ...)`

    provide different `name-symbol` formatting options `option-symbol ...`.
    each `element` is a formatting program.

### `my-cond` Layout Options

`my-cond` layout for the clauses:

- layout 1

    ```racket
    (cond <clause-1>
          <clause-2>
          ...)
    ```

- layout 2

    ```racket
    (cond
      <clause-1>
      <clause-2>
      ...)
    ```

`my-cond` layout for each of the clauses:

- layout 1 (default): preserve existing line breaks

- layout 2: Qs and As are always in the same line,
  separated by a space

    - layout 2.1: have Qs and As in the same line iff
      the clause contains exactly one answer

- layout 3: Qs and As are always in individual lines

## Misc old comments

- making the formatting a general-purpose programming language?

```
'(begin
   (define (provide-options-for-cond-like-indent
            proc)
     #(<>
       "("
       #(options
         'the-first-clause-follows-the-cond
         #(<> "my-cond" " " body)
         'the-first-clause-in-a-new-line
         #($$ "my-cond" #(nest 1 body)))
       ")"))

   #((require ...formatting-library...)
     (provide-options-for-cond-like-indent
      "my-cond"
      #($$ <clause> ...))
     )
   )
```

two possible formatting instructions for `let`

1. less tree-y

    ```racket
    #($$ #(<> "(" "let" " " bindings)
         #(nest 2 #(<> body ")")))
    ```

    result:

    ```
    |"(" "let" " (" |"[" a 10 ... "]"
                    |"[" ..... "]" ")"

    |__ body.... ")"
    ```
2. more tree-y

    ```racket
    #(<> "("
         #($$ #(<> "let" " " binding)
              #(nest 1 body))
         ")")
    ```

    result:

    ```
    "("|"let" " (" |"[" a 10 ... "]"
                   |"[" ..... "]" ")"
       |_body....         ")"
    ```

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

3. Figure out how to make combinators customizable for the user of the formatting tool,
   types of customization configured by the macro authors.

4. Add a test case for formatting the whole file.
    * What (top-level?) API should provided? -- `format-file : source-file -> dest-string`
    * Traverse fully expanded program to build a map from
      source locations to format instructions
    * Construct the formatted file by copying the source code.
      If encountered a source location that has formatting instruction,
      switch to print-formatted mode.
    * Handle nested format instructions. Currently, `print-formatted` interpret `#(source srcloc)` by copying the content at `srcloc` to the output. However, source codes at `srcloc` can also contain formatting instructions. An example:
        ```
        (my-cond (#t
                  (string-append "hello "
                                 (my-cond (else "world")))) (else 'not-here))
        ```
        Current output:
        ```
        (writeln
         (my-cond [#t
                   (string-append "hello "
                                  (my-cond (else "world")))]
                  [else 'not-here]))
        ```
