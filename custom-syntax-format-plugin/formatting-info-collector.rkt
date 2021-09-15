#lang racket/base

(require custom-syntax-format)

(provide get-formatting-info)

(define (get-formatting-info expanded-stx/exn path the-source orig-cust)
  (cond [(syntax? expanded-stx/exn)
         (define loc-info-map
           (build-loc-info-map expanded-stx/exn))
         (vector
          (syntax-source expanded-stx/exn)
          (for/hash ([(loc info) (in-hash loc-info-map)])
           (values loc
                   (loc-info (loc-info-format info)
                             (srcloc->vector (loc-info-srcloc info))))))]
        [else #f]))

(define (srcloc->vector s)
  (vector (srcloc-source s)
          (srcloc-line s)
          (srcloc-column s)
          (srcloc-position s)
          (srcloc-span s)))