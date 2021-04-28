#lang racket/base

(require racket/port
         racket/match
         "printing.rkt")

(provide (all-defined-out))

(define (racket-format stx)
  (with-output-to-string
    (λ ()
      (port-count-lines! (current-output-port))
      (print-formatted
       (construct-formatting-info stx)))))

(define (extract-name-syntax-maps stx
                                  #:check-disappeared-use? [check-disappeared-use? #f])
  (define table (make-hash))
  (define (do-traverse datum)
    (cond
      [(vector? datum)
       (for ([substx (in-vector datum)])
         (do-traverse substx))]
      [(pair? datum)
       (do-traverse (car datum))
       (do-traverse (cdr datum))]
      [(syntax? datum)
       (define name (syntax-property datum 'syncheck:format:name))
       (when name
         (hash-set! table name datum))
       (when check-disappeared-use?
         (define uses (syntax-property datum 'disappeared-use))
         (let loop ([use uses])
           (cond
             [(syntax? use)
              (do-traverse use)]
             [(pair? use)
              (loop (car use))
              (loop (cdr use))]
             [else (void)])))
       (do-traverse (syntax-e datum))]
      ;; TODO recursively traverse all compound data
      [else (void)]))

  (do-traverse stx)
  table)

(define (recursively-construct-formatting-info table pp-info)
  (match pp-info
    [(? string? s) s]
    [(? symbol? name)
     (define stx/#f
       (hash-ref table name (λ () #f)))
     (cond
       [(not stx/#f)
        (format "MISSING:~a" name)]
       [else
        (construct-formatting-info-from-syntax
         table
         stx/#f)])]
    [`#(<> ,elements ...)
     `#(<>
        ,@(for/list ([element (in-list elements)])
            (recursively-construct-formatting-info table element)))]
    [`#($$ ,elements ...)
     `#($$
        ,@(for/list ([element (in-list elements)])
            (recursively-construct-formatting-info table element)))]
    [`#(preserve-linebreak ,elements ...)
     `#(preserve-linebreak
        ,@(for/list ([element (in-list elements)])
            (recursively-construct-formatting-info table element)))]
    [`#(nest ,depth ,element)
     `#(nest ,depth
             ,(recursively-construct-formatting-info table element))]
    [`#(source ,source ,line ,col ,pos ,span)
     `#(source ,source ,line ,col ,pos ,span)]
    [`#(options ,name ,options ...)
     `#(options ,name ,@(for/list ([option (in-list options)])
                          (cons (car option)
                                (recursively-construct-formatting-info table (cdr option)))))]))

(define (construct-formatting-info-from-syntax table stx)
  (define pp-info
    (syntax-property stx 'syncheck:format))
  (cond
    [(not pp-info)
     `#(source ,(syntax-source stx)
               ,(syntax-line stx)
               ,(syntax-column stx)
               ,(syntax-position stx)
               ,(syntax-span stx))]
    [else
     (recursively-construct-formatting-info table pp-info)]))

(define (construct-formatting-info stx)
  (define expanded-stx (expand stx))
  (define name-stx-map
    (extract-name-syntax-maps
     expanded-stx
     #:check-disappeared-use? #t))
  (construct-formatting-info-from-syntax name-stx-map expanded-stx))
