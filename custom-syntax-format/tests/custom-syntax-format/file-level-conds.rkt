#lang racket/base

(require custom-syntax-format)

(writeln
 (my-cond (#f "false") [(and (< 10 5)
                             'okay)
                        "a"] (#t "b") (else
                                                     #f)
               ))
