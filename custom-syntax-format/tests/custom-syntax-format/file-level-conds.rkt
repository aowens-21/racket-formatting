#lang racket/base

(require custom-syntax-format)

(writeln
 (my-cond (#f "false") [(and (< 10 5)
                             'okay)
                        "a"] (#t "b") (else
                                                     #f)
               ))

(writeln
 (my-cond (#t
           (string-append "hello "
                          (my-cond (else "world")))) (else 'not-here)))
