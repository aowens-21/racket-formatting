#lang at-exp racket/base

(require racket/gui/base
         racket/class
         custom-syntax-format
         custom-syntax-format/gui-text
         syntax/modread
         framework)

(define t (new racket:text%))
(define f (new frame%
               [label "Testing Formatting"]
               [width 1100]
               [height 700]))
(define e (new editor-canvas%
               [parent f]
               [editor t]))

(define program-text
  @string-append{
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
})

(send t insert program-text)

(require pict
         pict/snip)

(send t insert
      (new pict-snip%
           [pict (circle 50 #:border-color "red" #:border-width 5)])
      65)

(define p (open-input-text-editor t
                                  0
                                  'end
                                  (λ (s)
                                    (special-value s))
                                  "untitled.rkt"
                                  #f))

(define stx
  ((λ (in)
    (port-count-lines! in)
    (with-module-reading-parameterization
      (λ () (read-syntax (object-name in) in))))
  p))

(define ns (make-empty-namespace))
(parameterize ([current-namespace ns])
  (namespace-attach-module (namespace-anchor->namespace
                            here-namespace-anchor)
                           ''#%builtin)
  (namespace-require '(only '#%kernel module module)))

(define expanded-stx
  (parameterize ([current-namespace ns])
    (expand stx)))

(define make-text-peek-procedure
  (text-peek-procedure-maker "untitled.rkt" t))

(define format-insts-reversed
  (reverse
   (format-syntax-in-range "untitled.rkt"
                           expanded-stx
                           1
                           (send t last-position)
                           make-text-peek-procedure
                           (current-output-port))))

(send f show #t)

#;(process-editor-instructions t format-insts-reversed)
