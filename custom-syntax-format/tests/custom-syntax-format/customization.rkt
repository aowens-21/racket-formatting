#lang at-exp racket/base

(require racket/pretty
         rackunit
         custom-syntax-format
         "example-forms.rkt")

(provide (all-defined-out))

#|
1. the programmers: the person implementing macros
2. the user(s): the person using the macros and the formatting tool
|#

(pretty-write
 (construct-formatting-info
  my-cond-stx.4))

#|
my-cond layout for the clauses:

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
|#

(pretty-write
 #(let ([body #($$ ...)])
    #(<>
      "("
      #(options
        'the-first-clause-follows-the-cond
        #(<> "my-cond" " " body)
        'the-first-clause-in-a-new-line
        #($$ "my-cond" #(nest 1 body)))
      ")"))
 )

#|
my-cond layout for each of the clauses:

- layout 1 (default): preserve existing line breaks

- layout 2: Qs and As are always in the same line,
  separated by a space

    - layout 2.1: have Qs and As in the same line iff
      the clause contains exactly one answer

- layout 3: Qs and As are always in individual lines
|#

(pretty-write
 #(let ([body
         #($$ #(<> "["
                   #(options
                     'default
                     #(preserve-linebreak #(source) #(source))
                     'same-line
                     #(<> #(source) " " #(source))
                     'force-line-break
                     #($$ #(source)
                          #(source)))
                   "]")
              ...)
         ])
    #(<>
      "("
      #(options
        'the-first-clause-follows-the-cond
        #(<> "my-cond" " " body)
        'the-first-clause-in-a-new-line
        #($$ "my-cond" #(nest 1 body)))
      ")"))
 )

#|
if going far towards making the formatting a general-purpose
programming language
|#

(pretty-write
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
    ))
