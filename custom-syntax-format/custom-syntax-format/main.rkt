#lang racket/base

(require racket/class
         racket/match
         racket/file
         racket/port
         "printing.rkt")

(provide (all-defined-out)
         racket-format-config
         current-racket-format-print-source
         default-racket-format-print-source
         format-write-char
         format-write-string
         start-copy
         stop-copy)

;; locations: source path × source position
(struct format-loc (source pos) #:prefab)
;; info: format instructions and the complete source locations
(struct loc-info (format srcloc) #:prefab)

(struct special-value (snip) #:transparent)

(struct write-inst (str) #:transparent)
(struct start-copy-inst (pos) #:transparent)
(struct stop-copy-inst (pos) #:transparent)

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
       (when (and name
                  (syntax-source datum)
                  (syntax-line datum)
                  (syntax-column datum)
                  (syntax-position datum)
                  (syntax-span datum))
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
      ;; TODO recursively traverse all compound data (boxes, etc...)
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
                       (recursively-construct-formatting-info (cdr option)))))]
         [`#(source/maybe-name ,source ,line ,col ,pos ,span ,name)
          (cond
            [(and name
                  (hash-has-key? name-to-srcloc-map name))
             (construct-formatting-info-from-location
              name-to-srcloc-map
              loc-info-map
              (hash-ref name-to-srcloc-map name))]
            [else
             `#(source ,source ,line ,col ,pos ,span)])]))]))

;; format-file : source-file config-file dest-file
;;=>
;; format-file : source-file -> dest-string

(require syntax/modread
         racket/pretty)

(define-namespace-anchor here-namespace-anchor)

(define (format-file filename)
  (with-output-to-string
    (λ ()
      (do-format-file filename (current-output-port)))))

(define (get-format-instructions filename)
  (define format-insts '())
  (parameterize ([format-write-char
                  (λ (ch . args)
                    (set! format-insts
                          (cons `(write-string ,(string ch))
                                format-insts))
                    (apply write-char ch args))]
                 [format-write-string
                  (λ (str . args)
                    (set! format-insts
                          (cons `(write-string ,str)
                                format-insts))
                    (apply write-string str args))]
                 [start-copy
                  (λ (pos)
                    (set! format-insts
                          (cons `(start-copy ,pos)
                                format-insts)))]
                 [stop-copy
                  (λ (pos)
                    (set! format-insts
                          (cons `(stop-copy ,pos)
                                format-insts)))])
    (do-format-file filename (open-output-nowhere)))
  (reverse format-insts))

(define (do-format-file filename output-port)
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

  (define file-content
    (file->string filename))
  (define (make-file-content-peek-procedure start-pos span)
    (unless (and (<= 1 start-pos) (<= start-pos (string-length file-content))
                 (<= (+ start-pos span) (+ 1 (string-length file-content))))
      (error 'make-file-content-peek-procedure
             (string-append "the specified range is not a subinterval of the file range"
                            "  specified range: [~a, ~a)\n"
                            "  file range: [~a, ~a)\n")
             start-pos
             (+ start-pos span)
             1
             (+ 1 (string-length file-content))))
    (define reading-pos start-pos)
    (λ (peek-pos)
      (unless (and (<= reading-pos peek-pos) (< peek-pos (+ start-pos span)))
        (error 'file-content-peek-procedure
               (string-append "peeking position is out of range\n"
                              "  position: ~a\n"
                              "  reading position: ~a\n"
                              "  range: [~a, ~a)\n")
               peek-pos
               reading-pos
               start-pos
               (+ start-pos span)))
      (set! reading-pos peek-pos)
      (string-ref file-content (- peek-pos 1))))

  (format-syntax-in-range filename
                          (build-loc-info-map expanded-stx)
                          1
                          (string-length file-content)
                          make-file-content-peek-procedure
                          output-port))

(define (format-syntax-in-range filename
                                loc-info-map
                                start-pos
                                span
                                make-peek-procedure
                                output-port)
  (port-count-lines! output-port)

  (define format-insts '())

  (parameterize ([current-output-port output-port]
                 [format-write-char
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
    (print-file-with-format
     filename
     start-pos
     span
     make-peek-procedure
     loc-info-map))

  (reverse format-insts))

(define (build-loc-info-map expanded-stx)
  (define names-to-srcloc-map
    (extract-name-srcloc-maps expanded-stx
                              #:check-disappeared-use? #t))
  ;; Build maps from locations to temporary format instructions
  ;;   locations: source path × source position
  ;;   temporary format instructions: format instructions with names (from
  ;;   syncheck:format:name) where the names are placeholders for the actual,
  ;;   recursive part of the format instructions
  (define loc-info-map/format-instructions-with-names
    (build-loc-info-with-names-map expanded-stx))
  ;; substitute the actual (recursive) format instructions for the names
  ;; in the temporary map
  (define loc-info-map
    (for/hash ([(loc info) (in-hash loc-info-map/format-instructions-with-names)])
      (values
       loc
       (loc-info
        (construct-formatting-info-from-location
         names-to-srcloc-map
         loc-info-map/format-instructions-with-names
         (loc-info-srcloc info))
        (loc-info-srcloc info)))))
  loc-info-map)

;; traverse the entire syntax object and build a map from locations
;; to temporary format instructions (stored in syncheck:format)
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

(define start-copy (make-parameter void))
(define stop-copy (make-parameter void))

;; Conceptually, the start-pos is the start position in the
;; source location. Therefore it starts counting from 1.
;;
;; The span is the length of the input.
(define (print-file-with-format-in-range
         filename
         peek-char-or-special-at-pos
         loc-info-map
         start-pos
         span
         #:shift-amount [shift-amount 0])
  ;; end-pos is exclusive. Either the character at end-pos should not be
  ;; processed, or end-pos may be one over the valid positions.
  (define end-pos (+ start-pos span))
  ((start-copy) start-pos)
  (let copy-loop! ([pos start-pos])
    (when (< pos end-pos)
      (define line-finish-pos
        (let copy-current-line! ([pos pos])
          (cond
            [(>= pos end-pos)
             pos]
            [(eq? (peek-char-or-special-at-pos pos) #\newline)
             pos]
            [else
             (define loc
               (format-loc filename pos))
             (cond
               [(hash-has-key? loc-info-map loc)
                (define loc-info-at-pos (hash-ref loc-info-map loc))
                (define span (srcloc-span (loc-info-srcloc loc-info-at-pos)))
                (define next-pos (+ pos span))
                ((stop-copy) pos)
                (indent-at-current-col
                 (λ () (print-formatted (loc-info-format loc-info-at-pos))))
                ((start-copy) next-pos)
                (copy-current-line! next-pos)]
               [else
                (define ch (peek-char-or-special-at-pos pos))
                (match ch
                  [(? char?)
                   (write-char ch)]
                  [(special-value snip)
                   (cond
                     [(port-writes-special? (current-output-port))
                      (write-special snip)]
                     [else
                      (write-string (send snip get-text
                                          0
                                          (send snip get-count)
                                          #t))])])
                (copy-current-line! (+ pos 1))])])))
      (cond
        [(and (< line-finish-pos end-pos)
              (eq? (peek-char-or-special-at-pos line-finish-pos) #\newline))
         #|
           The beginning of the next line:

           ........\n  <- line-finish-pos points to the #\newline character
           .....................|ABCD|(my-cond ...)EFGH
           ^                    |   1. loc with format instruction, or
           line-finish-pos + 1  |   2. newline or other chars
           |
           ~~~~~~~~~~~ space-count
         |#
         (define space-count
           (let space-count-loop ([pos (+ line-finish-pos 1)]
                                  [count 0])
             (cond
               [(>= pos end-pos)
                count]
               [(member (peek-char-or-special-at-pos pos) '(#\space #\tab) eq?)
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
                (when (and (eq? (peek-char-or-special-at-pos pos) #\newline)
                           (hash-has-key? loc-info-map loc))
                  (error 'print-file-with-format-in-range:copy-loop!:space-count-loop
                         (string-append
                          "discover formatting instructions at location ~s "
                          "but the source text is a linebreak")
                         loc))
                count])))
         (parameterize ([format-indentation
                         (max 0 (+ shift-amount space-count))])
           ((stop-copy) line-finish-pos)
           (print-formatted-newline)
           ((start-copy) (+ line-finish-pos 1 space-count)))
         ;; skip the #\newline character and the leading spaces
         (copy-loop! (+ line-finish-pos 1 space-count))]
        [else
         (copy-loop! line-finish-pos)])))
  ((stop-copy) end-pos))

(define (print-file-with-format filename
                                start-pos
                                overall-span
                                make-peek-procedure
                                loc-info-map)

  (define old-print-source (current-racket-format-print-source))
  (define (print-source-with-format srcloc)
    (define source (srcloc-source srcloc))
    (define col (srcloc-column srcloc))
    (define pos (srcloc-position srcloc))
    (define span (srcloc-span srcloc))
    (cond
      [(and (equal? filename source)
            ;; first position
            (<= start-pos pos)
            ;; first position
            (< pos (+ start-pos overall-span))
            ;; last position
            (< (+ pos span -1) (+ start-pos overall-span)))
       (indent-at-current-col
        (λ ()
          (print-file-with-format-in-range
           filename
           (make-peek-procedure pos span)
           loc-info-map
           pos
           span
           #:shift-amount (- (format-indentation) col))))]
      [else
       (old-print-source srcloc)]))

  (parameterize ([current-racket-format-print-source print-source-with-format])
    (print-file-with-format-in-range
     filename
     (make-peek-procedure start-pos overall-span)
     loc-info-map
     start-pos
     overall-span)))
