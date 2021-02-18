#lang racket/base

(require racket/match
         racket/file
         racket/port
         "syntax.rkt"
         "example-forms.rkt")

(provide (all-defined-out)
         (all-from-out "example-forms.rkt"))

(define (extract-name-location-maps stx
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
         (hash-set! table name (make-srcloc (syntax-source datum)
                                            (syntax-line datum)
                                            (syntax-column datum)
                                            (syntax-position datum)
                                            (syntax-span datum))))
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


(define (construct-formatting-info-from-location name-to-location-map
                                                 loc-to-format-map
                                                 loc)
  (cond
    [(not (hash-has-key? loc-to-format-map loc))
     `#(source ,(srcloc-source loc)
               ,(srcloc-line loc)
               ,(srcloc-column loc)
               ,(srcloc-position loc)
               ,(srcloc-span loc))]
    [else
     ;; TODO FIXME
     (hash-ref loc-to-format-map loc)
     #;
     (recursively-construct-formatting-info-from-location
      name-to-location-map
      loc-to-format-map
      (hash-ref loc-to-format-map loc))]))

(define format-indentation (make-parameter 0))

(define (print-formatted-newline)
  (newline)
  (for ([i (in-range (format-indentation))])
    (write-char #\space)))

;; FIXME handle spaces
(define (write-string/shift-indentation str shift-amount)
  (define lines
    (call-with-input-string str port->lines))
  (for ([(a-line idx) (in-indexed (in-list lines))])
    (define space-count-at-start
      (string-length
       (list-ref (cdr (regexp-match #rx"^([ \\t]*)" a-line))
                 0)))
    (parameterize ([format-indentation
                    (max 0 (+ shift-amount space-count-at-start))])
      (when (> idx 0)
        (print-formatted-newline))
      (write-string (substring a-line space-count-at-start)))))

;; maps some config options
(define racket-format-config (make-parameter (hash)))

(define (print-formatted pp-info)
  (match pp-info
    [(? string? s)
     (write-string s)]
    [`#(source ,source ,line ,col ,pos ,span)
     (indent-at-current-col
      (lambda ()
        (write-string/shift-indentation
         ;; Position and line locations are numbered from 1;
         (substring (file->string source)
                    (sub1 pos)
                    (sub1 (+ pos span)))
         ;; column locations are numbered from 0.
         (- (format-indentation) col))))]
    [`#(<> ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (print-formatted element))]
    [`#($$ ,element0 ,elements ...)
     (indent-at-current-col
      (lambda ()
        (print-formatted element0)
        (for ([(element idx) (in-indexed elements)])
          (print-formatted-newline)
          (print-formatted element))))]
    [`#($$)
     (void)]
    [`#(preserve-linebreak
        ,(and element0 `#(source ,source0 ,line0 ,col0 ,pos0 ,span0))
        ,(and elements `#(source ,source ,line ,col ,pos ,span)) ...)
     (indent-at-current-col
      (lambda ()
        (define previous-line-no (get-current-line-number))
        (print-formatted element0)
        (for/fold ([previous-line-span (- (get-current-line-number) previous-line-no)])
                  ([previous-line-number (in-list (cons line0 line))]
                   [current-line-number (in-list line)]
                   [element (in-list elements)])
          (cond
            [(> current-line-number (+ previous-line-number previous-line-span))
             (print-formatted-newline)]
            [else
             (write-char #\space)])
          (define current-line-no (get-current-line-number))
          (print-formatted element)
          (- (get-current-line-number) current-line-no))))]
    [`#(preserve-linebreak ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (write-char #\space))
       (print-formatted element))]
    [`#(nest ,(? exact-integer? depth) ,element)
     (for ([i (in-range depth)])
       (write-char #\space))
     (indent-at-current-col
      (lambda ()
        (print-formatted element)))]
    [`#(options ,name ,options ...)
     (define chosen-option (hash-ref (racket-format-config)
                                     name
                                     (lambda () (car (list-ref options 0)))))
     (define chosen-format (cdr (or (assoc chosen-option options)
                                    (list-ref options 0))))
     (print-formatted chosen-format)]))

(define (indent-at-current-col proc)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  (parameterize ((format-indentation col))
    (proc)))

(define (get-current-line-number)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  line)

(define (racket-format stx)
  (with-output-to-string
    (λ ()
      (port-count-lines! (current-output-port))
      (print-formatted
       (construct-formatting-info stx)))))


;; format-file : source-file config-file dest-file
;;=>
;; format-file : source-file -> dest-string

(require syntax/modread
         racket/pretty)

(define-namespace-anchor here-namespace-anchor) 

(define (format-file filename)
  (define stx
    (call-with-input-file
     filename
     (λ (in)
       (port-count-lines! in)
       (with-module-reading-parameterization
         (λ () (read-syntax filename in))))))


  (define ns (make-empty-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module (namespace-anchor->namespace
                              here-namespace-anchor)
                             ''#%builtin)
    (namespace-require '(only '#%kernel module module)))

  (define expanded-stx
    (parameterize ([current-namespace ns])
      (expand stx)))

  (define names-to-loc-map
    (extract-name-location-maps expanded-stx
                              #:check-disappeared-use? #t))
  (define loc-to-format-with-names-map (build-loc-to-format-map expanded-stx))
  (define loc-to-format-map
    (for/hash ([loc (in-hash-keys loc-to-format-with-names-map)])
      (values
       loc
       (construct-formatting-info-from-location
        names-to-loc-map
        loc-to-format-with-names-map
        loc))))

  (pretty-write loc-to-format-map)
  (void))

(define (build-loc-to-format-map stx)
  (define loc-to-format-map (make-hash))
  (let loop ([stx stx])
    (cond [(pair? stx)
           (loop (car stx))
           (loop (cdr stx))]
          [(syntax? stx)
           (define syncheck-format (syntax-property stx 'syncheck:format))
           (define source (syntax-source stx))
           (define line (syntax-line stx))
           (define col (syntax-column stx))
           (define pos (syntax-position stx))
           (define span (syntax-span stx))
           (when (and syncheck-format
                      source
                      line
                      col
                      pos
                      span)
             (hash-set! loc-to-format-map
                        (make-srcloc source
                                     line
                                     col
                                     pos
                                     span)
                        syncheck-format))
           (loop (syntax-e stx))]))
  loc-to-format-map)
