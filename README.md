# Racket Formatting

## Memo
### Terminology

- the _programmers_ : the person implementing macros
- the _user(s)_ : the person using the macros and the formatting tool

### What are our goals (for now)?

1. Figure out how expressive our formatting language is by making examples, this will also help
  illustrate how useful the tool is. Sub-goals:

    - Express different formatting styles for different macros (examples: `->*`, `->i`, `define`)
    - Can we compose syntax classes? (e.g. [function headers](https://docs.racket-lang.org/syntax/Library_Syntax_Classes_and_Literal_Sets.html#%28part._.Function_.Headers%29) for `define`)

2. Figure out how the programmers using our formatting language can abstract common pieces from
  formatting templates they write, and in general work on making formatting programs nicer to write.

    - Make the formatting options less fixed (i.e. our combinator that uses "format-time" information)
      to inform which formatting styles will be used

### TODOS:

- Come up with a few more examples of macros we'd like to put formatting instructions on. 
  These should be a little more complex than the `my-cond` and `my-let` examples.

- Have a combinator that uses "format-time" information and uses that to make decisions about
  how to format a particular part of the macro (ex. If a cond clause has more than X expressions
  inside it, break lines between the expressions, otherwise put everything on the same line).

- Try to extract common pieces/functionality of the formatting templates as we create more examples,
  maybe we can use this to build a little common library. This will also help us figure out how to
  make the formatting language more extensible.

### Possible Future Works

- The comment information is dropped entirely

- Customizable combinators should provide some information for forms that don't have
  a pretty printing specification (for example, DrRacket's default indentation rules)

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
5. Abstract over the naming pattern:

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

    * Parsing with the `named` syntax class automatically attaches
      fresh names to the pattern variables. The programmers can refer to the
      annotated syntax objects using the `.stx` attribute:

        ```
        (syntax-parse stx
         [(form ([lhs:named rhs:named] ...) body-expr:named ...+)
          (syntax-property
           (syntax/loc stx (let ([lhs.stx rhs.stx] ...) body-expr.stx ...))
           'syncheck:format
           ...
        ````

    * The format combinators automatically coerce the annotated syntax
      objects into their names when constructing the format instructions.
      The programmers can directly pass such syntax objects
      to the combinators.

        ```
        (<> "("
            ($$ (<> (symbol->string (syntax-e #'form))
                    " ("
                    (apply $$
                           (for/list ([lhs (in-list (syntax-e #'(lhs.stx ...)))]
                                      [rhs (in-list (syntax-e #'(rhs.stx ...)))])
                             (<> "[" lhs " " rhs "]")))
                    ")")
                (nest 1 (apply $$ (syntax-e #'(body-expr.stx ...)))))
            ")")
        ```

6. Implement a simple format template, e.g.

    * For `(my-cond (expr:named ...+) ...)`:

        ```
        (define body
          (quasiformat-template
           ($$
            (<> "[" (options cond-body-line-break
                             preserve         (preserve-linebreak expr1.stx expr2.stx ...)
                             same-line        (<> expr1.stx (~@ " " expr2.stx) ...)
                             force-line-break ($$ expr1.stx expr2.stx ...))
                "]")
            ...)))
        (define this-form (symbol->string (syntax-e #'form)))
        (syntax-property
         (syntax/loc stx (cond [expr1.stx expr2.stx ...] ...))
         'syncheck:format
         (quasiformat-template
          (<> "("
              (options cond-first-clause
                       same-line        (<> (unformat this-form) " "
                                            (format-embed (unformat body)))
                       force-line-break ($$ (unformat this-form)
                                            (nest 1 (format-embed (unformat body)))))
              ")")))
        ```

    * For `(my-let ([lhs:named rhs:named] ...) body-expr:named ...+)`:

        ```
        (quasiformat-template
          (<> "("
              ($$ (<> (unformat (symbol->string (syntax-e #'form)))
                      " ("
                      ($$ (<> "[" lhs.stx " " rhs.stx "]") ...)
                      ")")
                  (nest 1 ($$ body-expr.stx ...)))
              ")"))
        ```

