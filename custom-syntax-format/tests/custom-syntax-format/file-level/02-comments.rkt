#lang racket/base

(require custom-syntax-format/example-forms)

(writeln
 (my-cond
  (#t
   (string-append "hello " #;(s-expr comment)
           #| comment 3 |#
                    (my-cond (else "world")))) (else 'not-here)))

(writeln
 (;; a bad comment
  my-cond
  ;; cannot stay in the same line with my-cond
      (#| another comment |# #true
         "hi comments"
         ;; at the end
         )
      #;
  (a commented out clause)
  (else #|more comments|# "unreachable")))

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
