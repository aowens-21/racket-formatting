#lang racket/base

(require drracket/tool
         racket/unit
         custom-syntax-format
         racket/runtime-path)

(provide tool@)

(define-runtime-path formatting-info-collector "formatting-info-collector.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define loc-info-table #f)
    
    #;(define format-button-mixin
      (mixin (drracket:unit:frame<%>)
        (super-new)
        #f))

    (drracket:module-language-tools:add-online-expansion-handler
     formatting-info-collector
     'get-formatting-info
     (λ (defs-text loc-info-map)
       (define loc-info-map-with-srclocs
         (for/hash ([(loc info) (in-hash loc-info-map)])
           (values loc
                   (loc-info (loc-info-format info)
                             (apply srcloc (vector->list (loc-info-srcloc info)))))))
       (set! loc-info-table loc-info-map-with-srclocs)
       (printf "~s\n" loc-info-table)))))
       
        
    
