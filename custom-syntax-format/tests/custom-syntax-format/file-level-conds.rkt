#lang racket/base

(require custom-syntax-format)

(writeln
 (my-cond (#f "false") [(< 10 5)
                        "a"] (#t "b") (else
                                                     #f)
               ))
