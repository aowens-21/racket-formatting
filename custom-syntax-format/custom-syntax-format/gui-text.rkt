#lang racket/base

(require racket/gui/base
         racket/class
         custom-syntax-format)

(provide
 text-peek-procedure-maker
 process-editor-instructions)

(define (text-peek-procedure-maker filename text-editor)
  (define (make-text-peek-procedure start-pos span)
    (define current-port (open-input-text-editor text-editor
                                                 (- start-pos 1)
                                                 (+ (- start-pos 1)
                                                    span)
                                                 (λ (s)
                                                   (special-value s))
                                                 filename
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
  make-text-peek-procedure)

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
    (cond [(write-inst? a-inst)
           (define original-pos (send a-text get-end-position))
           (send a-text insert (write-inst-str a-inst))
           (send a-text set-position original-pos)]
          [(start-copy-inst? a-inst)
           (define corresponding-stop (hash-ref start-stop-pairs
                                                a-inst))
           (define start-pos (stop-copy-inst-pos corresponding-stop))
           (define end-pos (start-copy-inst-pos a-inst))
           (send a-text delete (- start-pos 1) (- end-pos 1))
           (send a-text set-position (- start-pos 1))])))
