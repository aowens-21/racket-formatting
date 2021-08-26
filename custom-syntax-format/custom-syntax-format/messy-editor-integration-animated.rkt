#lang at-exp racket/base

(require racket/gui/base
         racket/class
         custom-syntax-format
         syntax/modread
         framework)

(define INSERT-DELAY 0.06)
(define PAUSE-DELAY 1.0)

(define t (new racket:text%))
(define f (new frame%
               [label "Testing Formatting"]
               [width 900]
               [height 400]))
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

(struct special-snip-value () #:transparent)

(define p (open-input-text-editor t
                                  0
                                  'end
                                  (λ (s)
                                    (special-snip-value))
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

(define (make-text-peek-procedure start-pos span)
  (define current-port (open-input-text-editor t
                                               (- start-pos 1)
                                               (+ (- start-pos 1)
                                                  span)
                                               (λ (s)
                                                 (special-snip-value))
                                               "untitled.rkt"
                                               #f))
  (port-count-lines! current-port)
  (λ (peek-pos)
    (let loop ()
      (define-values (line col rel-curr-pos)
        (port-next-location current-port))
      (define curr-pos (+ rel-curr-pos start-pos -1))
      (when (< curr-pos peek-pos)
        (read-char-or-special current-port)
        (loop)))
    (peek-char-or-special current-port)))

(define format-insts '())

(struct write-inst (str) #:transparent)
(struct start-copy-inst (pos) #:transparent)
(struct stop-copy-inst (pos) #:transparent)

(parameterize ([format-write-char
                (λ (ch . args)
                  (set! format-insts
                        (cons (write-inst (string ch))
                              format-insts))
                  (apply write-char ch args))]
               [format-write-string
                (λ (str . args)
                  (set! format-insts
                        (cons (write-inst str)
                              format-insts))
                  (apply write-string str args))]
               [start-copy
                (λ (pos)
                  (set! format-insts
                        (cons (start-copy-inst pos)
                              format-insts)))]
               [stop-copy
                (λ (pos)
                  (set! format-insts
                        (cons (stop-copy-inst pos)
                              format-insts)))])
  (format-syntax-in-range "untitled.rkt"
                          expanded-stx
                          1
                          (send t last-position)
                          make-text-peek-procedure
                          (current-output-port)))

(define (process-editor-instructions a-text instructions)
  (define start-stop-pairs (make-hash))
  (define first-start-copy
    (for/fold ([previous-start 'done])
            ([a-inst (in-list instructions)])
    (cond [(write-inst? a-inst)
           previous-start]
          [(start-copy-inst? a-inst)
           a-inst]
          [(stop-copy-inst? a-inst)
           (hash-set! start-stop-pairs
                      previous-start
                      a-inst)
           'middle])))
  (hash-set! start-stop-pairs
             first-start-copy
             (stop-copy-inst (start-copy-inst-pos first-start-copy)))
  (for ([a-inst (in-list instructions)])
    (unless (write-inst? a-inst)
      (println a-inst))
    (cond [(write-inst? a-inst)
           (define original-pos (send t get-end-position))
           (sleep/yield INSERT-DELAY)
           (send t insert (write-inst-str a-inst))
           (send t set-position original-pos)]
          [(start-copy-inst? a-inst)
           (define corresponding-stop (hash-ref start-stop-pairs
                                                a-inst))
           (define start-pos (stop-copy-inst-pos corresponding-stop))
           (define end-pos (start-copy-inst-pos a-inst))
           (sleep/yield (* PAUSE-DELAY 1.5))
           (printf "    delete [~a,~a]\n" (- start-pos 1) (- end-pos 1))
           (send t set-position (- start-pos 1) (- end-pos 1))
           (sleep/yield PAUSE-DELAY)
           (send t delete (- start-pos 1) (- end-pos 1))
           (send t set-position (- start-pos 1))
           (printf "    set-position to ~a\n" (- start-pos 1))
           (sleep/yield PAUSE-DELAY)])))

(module+ main
  (send f show #t)
  (queue-callback
   (λ ()
     (printf "wait for 2 seconds\n")
     (sleep/yield 2)
     (send t own-caret #t)
     (printf "start\n")
     (send e force-display-focus #t)
     (process-editor-instructions t format-insts)
     (send e force-display-focus #f)
     (printf "done\n"))
   #f)
  (yield 'wait))
