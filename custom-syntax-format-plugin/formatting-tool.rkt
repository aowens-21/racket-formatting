#lang racket/base

(require drracket/tool
         racket/unit
         custom-syntax-format
         custom-syntax-format/gui-text
         racket/runtime-path
         racket/class
         pict
         mrlib/switchable-button)

(provide tool@)

(define-runtime-path formatting-info-collector "formatting-info-collector.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define loc-info-table #f)
    (define file-path #f)

    (define (format-range-in-buffer a-text)
      (define selected-start (send a-text get-start-position))
      (define selected-end (send a-text get-end-position))
      (define-values (start end)
        (if (= selected-start selected-end)
            (values 1 (send a-text last-position))
            (values (+ selected-start 1) selected-end)))

      
      (define make-text-peek-procedure
        (text-peek-procedure-maker file-path a-text))

      (define format-insts-reversed
        (reverse
         (format-syntax-in-range file-path
                                 loc-info-table
                                 start
                                 (- end start)
                                 make-text-peek-procedure
                                 (current-output-port))))

      (send a-text begin-edit-sequence)
      (process-editor-instructions a-text format-insts-reversed)
      (send a-text end-edit-sequence))
    
    (define format-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
 
        (let ((btn
               (new switchable-button%
                    (label "Auto-format")
                    (callback (λ (button)
                                (format-range-in-buffer (get-definitions-text))))
                    (parent (get-button-panel))
                    (bitmap (pict->bitmap (circle 10))))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))

    (drracket:module-language-tools:add-online-expansion-handler
     formatting-info-collector
     'get-formatting-info
     (λ (defs-text loc-info-vec)
       (define loc-info-map-with-srclocs
         (for/hash ([(loc info) (in-hash (vector-ref loc-info-vec 1))])
           (values loc
                   (loc-info (loc-info-format info)
                             (apply srcloc (vector->list (loc-info-srcloc info)))))))
       (set! file-path (vector-ref loc-info-vec 0))
       (set! loc-info-table loc-info-map-with-srclocs)))
    (drracket:get/extend:extend-unit-frame format-button-mixin)))
       
        
    
