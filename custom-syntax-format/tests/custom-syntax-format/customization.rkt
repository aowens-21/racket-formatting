#lang at-exp racket/base

(require rackunit
         custom-syntax-format
         custom-syntax-format/syntax
         "example-forms.rkt")

(provide (all-defined-out))

(define my-cond-expected.1/all-same-line
  @string-append{
 (my-cond [#f "false"]
          [(< 10 5) "a"]
          [#t "b"]
          [else #f])
 })

(module+ test
  (check-equal?
   (parameterize ([racket-format-config
                   (hash 'cond-first-clause 'same-line
                         'cond-body-line-break 'same-line)])
     (racket-format my-cond-stx.1))
   my-cond-expected.1/all-same-line))

(define my-cond-expected.3/all-force-line-break
  @string-append{
 (my-cond
   [#t
    "ans1"
    "answer2"
    'answer3
    (hash 'answer4 #t)]
   [else
    #t])
 })

(module+ test
  (check-equal?
   (parameterize ([racket-format-config
                   (hash 'cond-first-clause 'force-line-break
                         'cond-body-line-break 'force-line-break)])
     (racket-format my-cond-stx.3))
   my-cond-expected.3/all-force-line-break))

(define my-cond-expected.4/first-clause-force-line-break
  @string-append{
 (my-cond
   [(+
     5
     3) 'ok]
   [else #t])
 })

(define my-cond-expected.4/body-force-line-break
  @string-append{
 (my-cond [(+
            5
            3)
           'ok]
          [else
           #t])
 })

(define my-cond-expected.4/all-force-line-break
  @string-append{
 (my-cond
   [(+
     5
     3)
    'ok]
   [else
    #t])
 })

(module+ test
  (check-equal?
   (parameterize ([racket-format-config
                   (hash 'cond-first-clause 'force-line-break
                         'cond-body-line-break 'preserve)])
     (racket-format my-cond-stx.4))
   my-cond-expected.4/first-clause-force-line-break)

  (check-equal?
   (parameterize ([racket-format-config
                   (hash 'cond-first-clause 'same-line
                         'cond-body-line-break 'force-line-break)])
     (racket-format my-cond-stx.4))
   my-cond-expected.4/body-force-line-break)

  (check-equal?
   (parameterize ([racket-format-config
                   (hash 'cond-first-clause 'force-line-break
                         'cond-body-line-break 'force-line-break)])
     (racket-format my-cond-stx.4))
   my-cond-expected.4/all-force-line-break))
