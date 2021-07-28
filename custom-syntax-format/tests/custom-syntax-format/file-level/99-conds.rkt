#lang racket/base

(require custom-syntax-format/example-forms)

(writeln
 (my-cond (#t
              (string-append "hello " #;(s-expr comment)
                   #| comment 3 |#
                          (my-cond (else "world"))))
   (else (+ 2 3
            4)
            '|not-h
                 er
               e|)))
