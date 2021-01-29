#lang at-exp racket/base

(require rackunit
         custom-syntax-format)

(provide (all-defined-out))

(define my-cond-stx.1
   #'(my-cond (#f "false") [(< 10 5)
                            "a"] (#t "b") (else
                                                    #f)
              ))

(define my-cond-expected.1
  @string-append{
 (my-cond [#f "false"]
          [(< 10 5)
           "a"]
          [#t "b"]
          [else
           #f])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.1)
   my-cond-expected.1))

(define my-cond-stx.2
   #'(my-cond ((my-cond ((not #f)
                         'is-true)))
              (else
             #f)))

(define my-cond-expected.2
  @string-append{
 (my-cond [(my-cond [(not #f)
                     'is-true])]
          [else
           #f])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.2)
   my-cond-expected.2))

(define my-cond-stx.3
  #'(my-cond (#t "ans1"
               "answer2" 'answer3



                     (hash 'answer4 #t)) (else #t)))

(define my-cond-expected.3
  @string-append{
 (my-cond [#t "ans1"
           "answer2" 'answer3
           (hash 'answer4 #t)]
          [else #t])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.3)
   my-cond-expected.3))

(define my-cond-stx.4
  #'(my-cond ((+
               5
               3) 'ok)
             (else #t)))

(define my-cond-expected.4
  @string-append{
 (my-cond [(+
            5
            3) 'ok]
          [else #t])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.4)
   my-cond-expected.4))

(define my-cond-stx.5
  #'(my-cond ((+
               5
               3)
              'ok)
             (else #t)))

(define my-cond-expected.5
  @string-append{
 (my-cond [(+
            5
            3)
           'ok]
          [else #t])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.5)
   my-cond-expected.5))

(define my-cond-stx.6
  #'(my-cond (
              "Q" 'ok)
             (else #t)))

(define my-cond-expected.6
  @string-append{
 (my-cond ["Q" 'ok]
          [else #t])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.6)
   my-cond-expected.6))

(define my-cond-stx.7
   #'(my-cond (#f "false") [(< 10
                               5)
                            "a"] (#t "b") (else
                                                    #f)
              ))

(define my-cond-expected.7
  @string-append{
 (my-cond [#f "false"]
          [(< 10
              5)
           "a"]
          [#t "b"]
          [else
           #f])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.7)
   my-cond-expected.7))

(define my-let-stx.1
  #'(my-let ([a 10] [b 5] [c
                       20])
         (+ a b c)))

(define my-let-expected.1
  @string-append{
 (my-let ([a 10]
          [b 5]
          [c 20])
   (+ a b c))
 })

(module+ test
  (check-equal?
   (racket-format my-let-stx.1)
   my-let-expected.1))

(define my-let-stx.2
  #'(my-let ([a 10] [b 5] [c
                       20])
         (+ a b
              c)))

(define my-let-expected.2
  @string-append{
 (my-let ([a 10]
          [b 5]
          [c 20])
   (+ a b
        c))
 })

(module+ test
  (check-equal?
   (racket-format my-let-stx.2)
   my-let-expected.2))

(define my-let-stx.3
  #'(my-let ([a 10] [b 5] [c
                       (* 7
                          3)])
         (+ a b c)))

(define my-let-expected.3
  @string-append{
 (my-let ([a 10]
          [b 5]
          [c (* 7
                3)])
   (+ a b c))
 })

(module+ test
  (check-equal?
   (racket-format my-let-stx.3)
   my-let-expected.3))

(define my-let-stx.4
  #'(my-let ([a 10] [b 5] [c
                       (* 7
                          3)])
         (+ a b
              c)))

(define my-let-expected.4
  @string-append{
 (my-let ([a 10]
          [b 5]
          [c (* 7
                3)])
   (+ a b
        c))
 })

(module+ test
  (check-equal?
   (racket-format my-let-stx.4)
   my-let-expected.4))
