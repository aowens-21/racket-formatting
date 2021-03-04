#lang racket/base

(require racket/sequence
         racket/match
         syntax/parse)

(provide (all-defined-out))

(define (attach-name stx [base "g"])
  (define name (gensym base))
  (values (syntax-property stx 'syncheck:format:name name)
          name))

(define-syntax-class named
  (pattern _
           #:with fresh-name (datum->syntax #f (gensym "g"))
           #:attr name #'fresh-name
           #:attr stx (syntax-property this-syntax
                                       'syncheck:format:name
                                       (syntax-e #'fresh-name))))

(define (coerce-format value)
  (match value
    [(? syntax? named-stx)
     #:when (syntax-property named-stx 'syncheck:format:name)
     (syntax-property named-stx 'syncheck:format:name)]
    [(? string? str-lit)
     str-lit]
    [(? symbol? name)
     name]
    [(? vector? format)
     format]))

(define (<> . elems)
  `#(<> ,@(map coerce-format elems)))

(define ($$ . elems)
  `#($$ ,@(map coerce-format elems)))

(define (preserve-linebreak . elems)
  `#(preserve-linebreak ,@(map coerce-format elems)))

(define (nest indent-depth elem)
  `#(nest ,indent-depth ,(coerce-format elem)))

(define (options name . opts)
  `#(options
     ,name
     ,@(for/list ([an-option (in-slice 2 (in-list opts))])
         (cons (list-ref an-option 0)
               (coerce-format (list-ref an-option 1))))))
