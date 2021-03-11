#lang racket/base

(require custom-syntax-format)

;; comment1
(writeln
 ;; comment2
 (my-cond (#f "false") [(and (< 10 5)
                             'okay)
                        "a"] (#t "b") (else
                                                     #f)
               ))

(writeln
 (my-cond (#t
           (string-append "hello " #;(s-expr comment)
                   #| comment 3 |#
                          (my-cond (else "world")))) (else 'not-here)))

(writeln
 (my-cond ((+ 2 3 4
              5)
             #;(doesnt exist)
                ;; A disppearing comment
         #;
              (other s-expr comment)
     #|
    Some
  indented
      comment
 |#
              'whatever
                   (string-append "hel"
                                  "lo " (my-cond ((+ 6 7
                                                     8)
                                                  "wo") (else #f)) "rl"
                                            "d w/bad indentation")) (else 'not-here)))
