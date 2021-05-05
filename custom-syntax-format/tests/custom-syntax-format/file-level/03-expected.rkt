#lang racket/base

(require custom-syntax-format/example-forms
         racket/contract)

(my:->* ((my:->* (boolean?)
                 any))
        (values any/c))
