#lang racket/base

(require racket/sequence)

(provide (all-defined-out))

(define (attach-name stx [base "g"])
  (define name (gensym base))
  (values (syntax-property stx 'syncheck:format:name name)
          name))

(define (<> . elems)
  `#(<> ,@elems))

(define ($$ . elems)
  `#($$ ,@elems))

(define (preserve-linebreak . elems)
  `#(preserve-linebreak ,@elems))

(define (nest indent-depth elem)
  `#(nest ,indent-depth ,elem))

(define (options name . opts)
  `#(options
     ,name
     ,@(for/list ([an-option (in-slice 2 (in-list opts))])
         (cons (list-ref an-option 0)
               (list-ref an-option 1)))))
