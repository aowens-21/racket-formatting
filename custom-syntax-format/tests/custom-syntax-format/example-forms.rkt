#lang at-exp racket/base

(require rackunit
         custom-syntax-format/syntax
         custom-syntax-format/example-forms)

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

(define my-cond-stx.8
  #'(my-cond ((> 2 1)
              #t)
             (#f 'false)))

(define my-cond-expected.8
  @string-append{
 (my-cond [(> 2 1)
           #t]
          [#f 'false])
 })

(module+ test
  (check-equal?
   (racket-format my-cond-stx.8)
   my-cond-expected.8))

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

(define my-arrow-star-stx.0
  #'(my:->* (
          ) number?))

(define my-arrow-star-expected.0
  @string-append{
 (my:->* ()
         number?)
})

(module+ test
  (check-equal?
   (racket-format my-arrow-star-stx.0)
   my-arrow-star-expected.0))

(define (is-odd-number? value)
  (and (integer? value)
       (odd? value)))

(define my-arrow-star-stx.0.5
  #'(my:->* (
          ) is-odd-number?))

(define my-arrow-star-expected.0.5
  @string-append{
 (my:->* ()
         is-odd-number?)
})

(module+ test
  (check-equal?
   (racket-format my-arrow-star-stx.0.5)
   my-arrow-star-expected.0.5))

(define my-arrow-star-stx.1
  #'(my:->* (
          ) any))

(define my-arrow-star-expected.1
  @string-append{
 (my:->* ()
         any)
})

(module+ test
  (check-equal?
   (racket-format my-arrow-star-stx.1)
   my-arrow-star-expected.1))

(define my-arrow-star-stx.2
  #'(my:->* (number? (or/c #f (listof string?))) (values number?
  string?)))

(define my-arrow-star-expected.2
  @string-append{
 (my:->* (number?
          (or/c #f (listof string?)))
         (values number?
                 string?))
 })

(module+ test
  (check-equal?
   (racket-format my-arrow-star-stx.2)
   my-arrow-star-expected.2))

(define my-arrow-star-stx.3
  #'(my:->*
     (
        (my:->*
         (boolean?) any)
     )
     (values any/c)))

(define my-arrow-star-expected.3
  @string-append{
 (my:->* ((my:->* (boolean?)
                  any))
         (values any/c))
 })

(module+ test
  (check-equal?
   (racket-format my-arrow-star-stx.3)
   my-arrow-star-expected.3))

(define my-arrow-star-stx.4
  #'(my:->* (number? (->
      any)) ((or/c #f (listof string?)) (->
                                            any)) (values number?
  string?)))

(define my-arrow-star-expected.4
  @string-append{
 (my:->* (number?
          (->
 any))
         ((or/c #f (listof string?))
          (->
              any))
         (values number?
                 string?))
})

(module+ test
  (check-equal?
   (racket-format my-arrow-star-stx.4)
   my-arrow-star-expected.4))

(define my-arrow-stx.0
  #'(my:-> integer? integer? boolean?))

(define my-arrow-expected.0
  @string-append{
 (my:-> integer? integer? boolean?)
 })

(module+ test
  (check-equal?
   (racket-format my-arrow-stx.0)
   my-arrow-expected.0))

(define my-arrow-stx.1
  #'(my:-> integer? (values any/c boolean?)))

(define my-arrow-expected.1
  @string-append{
 (my:-> integer?
        (values any/c
                boolean?))
 })

#|
(module+ test
  (check-equal?
   (racket-format my-arrow-stx.1)
   my-arrow-expected.1))
|#

(define my-arrow-stx.2
  #'(my:-> (or/c 'apple 'orange 'pear 'cherry 'lemon 'pineapple) integer?))

(define my-arrow-expected.2
  @string-append{
 (my:-> (or/c 'apple 'orange 'pear 'cherry 'lemon 'pineapple)
        integer?)
 })

#|
(module+ test
  (check-equal?
   (racket-format my-arrow-stx.2)
   my-arrow-expected.2))
|#

(define my-arrow-stx.3
  #'(my:-> (my:-> (or/c 'apple 'orange 'pear 'cherry 'lemon 'pineapple) integer?)
           integer?))

(define my-arrow-expected.3
  @string-append{
 (my:-> (my:-> (or/c 'apple 'orange 'pear 'cherry 'lemon 'pineapple)
               integer?)
        integer?)
 })

#|
(module+ test
  (check-equal?
   (racket-format my-arrow-stx.3)
   my-arrow-expected.3))
|#

(define my-arrow-stx.4
  #'(my:-> string? (or/c 'apple 'orange) (or/c 'lemon 'pineapple) integer?))

(define my-arrow-expected.4
  @string-append{
 (my:-> string?
        (or/c 'apple 'orange)
        (or/c 'lemon 'pineapple)
        integer?)
 })

#|
(module+ test
  (check-equal?
   (racket-format my-arrow-stx.4)
   my-arrow-expected.4))
|#
