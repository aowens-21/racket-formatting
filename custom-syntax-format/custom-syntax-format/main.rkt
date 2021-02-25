#lang racket/base

(require racket/match
         racket/file
         racket/port
         "syntax.rkt"
         "example-forms.rkt")

(provide (all-defined-out)
         (all-from-out "example-forms.rkt"))

(struct format-loc (source pos) #:transparent)
(struct loc-info (format srcloc) #:transparent)

(define (srcloc->loc srcloc)
  (format-loc (srcloc-source srcloc)
               (srcloc-position srcloc)))

(define (extract-name-srcloc-maps stx
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

(define (construct-formatting-info-from-location name-to-srcloc-map
                                                 loc-info-map
                                                 srcloc)
  (define loc (srcloc->loc srcloc))
  (cond
    [(not (hash-has-key? loc-info-map loc))
     `#(source ,(srcloc-source srcloc)
               ,(srcloc-line srcloc)
               ,(srcloc-column srcloc)
               ,(srcloc-position srcloc)
               ,(srcloc-span srcloc))]
    [else
     (define format-at-loc
       (loc-info-format
        (hash-ref loc-info-map loc)))
     (let recursively-construct-formatting-info ([format format-at-loc])
       (match format
         [(? string? s) s]
         [(? symbol? name)
          (define srcloc/#f
            (hash-ref name-to-srcloc-map name (λ () #f)))
          (cond
            [(not srcloc/#f)
             (format "MISSING:~a" name)]
            [else
             (construct-formatting-info-from-location
              name-to-srcloc-map
              loc-info-map
              srcloc/#f)])]
         [`#(<> ,elements ...)
          `#(<>
             ,@(for/list ([element (in-list elements)])
                 (recursively-construct-formatting-info element)))]
         [`#($$ ,elements ...)
          `#($$
             ,@(for/list ([element (in-list elements)])
                 (recursively-construct-formatting-info element)))]
         [`#(preserve-linebreak ,elements ...)
          `#(preserve-linebreak
             ,@(for/list ([element (in-list elements)])
                 (recursively-construct-formatting-info element)))]
         [`#(nest ,depth ,element)
          `#(nest ,depth
                  ,(recursively-construct-formatting-info element))]
         [`#(options ,name ,options ...)
          `#(options
             ,name
             ,@(for/list ([option (in-list options)])
                 (cons (car option)
                       (recursively-construct-formatting-info (cdr option)))))]))]))

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

(define (default-racket-format-print-source srcloc)
  (define source (srcloc-source srcloc))
  (define pos (srcloc-position srcloc))
  (define span (srcloc-span srcloc))
  (define col (srcloc-column srcloc))
  (indent-at-current-col
   (lambda ()
     (write-string/shift-indentation
      ;; Position and line locations are numbered from 1;
      (substring (file->string source)
                 (sub1 pos)
                 (sub1 (+ pos span)))
      ;; column locations are numbered from 0.
      (- (format-indentation) col)))))

;; maps some config options
(define racket-format-config (make-parameter (hash)))

;; function to print a block of source text
(define current-racket-format-print-source (make-parameter default-racket-format-print-source))

(define (print-formatted pp-info)
  (match pp-info
    [(? string? s)
     (write-string s)]
    [`#(source ,source ,line ,col ,pos ,span)
     ((current-racket-format-print-source)
      (make-srcloc source
                   line
                   col
                   pos
                   span))]
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
    (call-with-input-file filename
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

  (define loc-info-map
    (build-loc-info-map expanded-stx))

  (with-output-to-string
    (λ ()
      (port-count-lines! (current-output-port))
      (print-file-with-format
       filename
       loc-info-map))))

(define (build-loc-info-map expanded-stx)
  (define names-to-srcloc-map
    (extract-name-srcloc-maps expanded-stx
                              #:check-disappeared-use? #t))
  (define loc-info-with-names-map
    (build-loc-info-with-names-map expanded-stx))
  (define loc-info-map
    (for/hash ([(loc info) (in-hash loc-info-with-names-map)])
      (values
       loc
       (loc-info
        (construct-formatting-info-from-location
         names-to-srcloc-map
         loc-info-with-names-map
         (loc-info-srcloc info))
        (loc-info-srcloc info)))))
  loc-info-map)

(define (build-loc-info-with-names-map expanded-stx)
  (define loc-info-with-names-map (make-hash))
  (let loop ([stx expanded-stx])
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
             (define srcloc
               (make-srcloc source
                            line
                            col
                            pos
                            span))
             (hash-set! loc-info-with-names-map
                        (srcloc->loc srcloc)
                        (loc-info syncheck-format srcloc)))
           (loop (syntax-e stx))]))
  loc-info-with-names-map)

(define (print-file-with-format-in-range
         filename
         content
         loc-info-map
         start-pos
         span
         #:shift-amount [shift-amount 0])
  (define end-pos (+ start-pos span))
  (let copy-loop! ([line-idx 0]
                   [pos start-pos])
    (when (< pos end-pos)
      #|
          ...........|ABCD|(my-cond ...)EFGH
          ^          |   1. loc with format isntruction, or
         pos         |   2. newline or other chars
          |
          ~~~~~~~~~~~ space-count
      |#
      (define space-count
        (let space-count-loop ([pos pos]
                               [count 0])
          (cond
            [(>= pos end-pos)
             count]
            [(member (string-ref content (- pos 1)) '(#\space #\tab) char=?)
             (define loc (format-loc filename pos))
             (when (hash-has-key? loc-info-map loc)
               (error 'print-file-with-format-in-range:copy-loop!:space-count-loop
                      (string-append
                       "discover formatting instructions at location ~s "
                       "but the source texts are spaces")
                      loc))
             (space-count-loop (+ 1 pos) (+ count 1))]
            [else
             (define loc (format-loc filename pos))
             (when (and (char=? (string-ref content (- pos 1)) #\newline)
                        (hash-has-key? loc-info-map loc))
               (error 'print-file-with-format-in-range:copy-loop!:space-count-loop
                      (string-append
                       "discover formatting instructions at location ~s "
                       "but the source text is a linebreak")
                      loc))
             count])))
      (when (> line-idx 0)
        (parameterize ([format-indentation
                        (max 0 (+ shift-amount space-count))])
          (print-formatted-newline)))
      (define line-finish-pos
        (let copy-current-line! ([pos (+ pos space-count)])
          (cond
            [(>= pos end-pos)
             pos]
            [(char=? (string-ref content (- pos 1)) #\newline)
             (+ 1 pos)]
            [else
             (define loc
               (format-loc filename pos))
             (cond
               [(hash-has-key? loc-info-map loc)
                (define loc-info-at-pos (hash-ref loc-info-map loc))
                (define span (srcloc-span (loc-info-srcloc loc-info-at-pos)))
                (define next-pos (+ pos span))
                (indent-at-current-col
                 (λ () (print-formatted (loc-info-format loc-info-at-pos))))
                (copy-current-line! next-pos)]
               [else
                (define ch (string-ref content (- pos 1)))
                (write-char ch)
                (copy-current-line! (+ pos 1))])])))
      (copy-loop! (+ 1 line-idx) line-finish-pos))))

(define (print-file-with-format filename loc-info-map)
  (define file-content
    (file->string filename))

  (define old-print-source (current-racket-format-print-source))
  (define (print-source-with-format srcloc)
    (define source (srcloc-source srcloc))
    (define col (srcloc-column srcloc))
    (define pos (srcloc-position srcloc))
    (define span (srcloc-span srcloc))
    (cond
      [(and (equal? filename source)
            ;; first position
            (<= 1 pos)
            ;; first position
            (<= pos (string-length file-content))
            ;; last position
            (<= (+ pos span -1) (string-length file-content)))
       (indent-at-current-col
        (λ ()
          (print-file-with-format-in-range
           filename
           file-content
           loc-info-map
           pos
           span
           #:shift-amount (- (format-indentation) col))))]
      [else
       (old-print-source srcloc)]))

  (parameterize ([current-racket-format-print-source print-source-with-format])
    (print-file-with-format-in-range
     filename
     file-content
     loc-info-map
     1
     (string-length file-content))))
