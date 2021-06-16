#lang racket/base

(require racket/match
         racket/string
         racket/pretty
         racket/file
         racket/port)

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define format-indentation (make-parameter 0))
(define format-columns (make-parameter (pretty-print-columns)))

;; TODO
(define (format-expected-max-column pp-info)
  (match pp-info
    [(? string? s)
     (cond [(non-empty-string? s)
            (+ 1 (for/sum ([ch (in-string s)]
                           #:when (char=? ch #\newline))
                   1))]
           [else 0])]
    [`#(source ,source ,line ,col ,pos ,span)
     ;; TODO
     ((current-racket-format-print-source)
      (make-srcloc source
                   line
                   col
                   pos
                   span))]
    [`#(<> ,elements ...)
     (define min-lines
       (for/list ([element (in-list elements)])
         (format-expected-min-lines element)))
     (+ 1 (for/sum ([min-line (in-list min-lines)]
                    #:when (not (zero? min-line)))
            (- min-line 1)))]
    [`#($$ ,element0 ,elements ...)
     (define min-lines
       (for/list ([element (in-list (cons element0 elements))])
         (format-expected-min-lines element)))
     (for/sum ([min-line (in-list min-lines)])
       (cond [(zero? min-line) 1]
             [else             min-line]))]
    [`#($$)
     0]
    [`#(preserve-linebreak
        ,(and element0 `#(source ,source0 ,line0 ,col0 ,pos0 ,span0))
        ,(and elements `#(source ,source ,line ,col ,pos ,span)) ...)
     ;; TODO
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
     ;; TODO
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (write-char #\space))
       (print-formatted element))]
    [`#(nest ,(? exact-integer? depth) ,element)
     (parameterize ([format-indentation (+ (format-indentation) depth)])
       (format-expected-min-lines element))]
    [`#(options ,name ,options ...)
     (define chosen-option (hash-ref (racket-format-config)
                                     name
                                     (lambda () (car (list-ref options 0)))))
     (define chosen-format (cdr (or (assoc chosen-option options)
                                    (list-ref options 0))))
     (format-expected-min-lines chosen-format)]))

(module+ test
  (check-equal?
   (format-expected-min-lines
    `#(<> "abc" "def"))
   1)
  (check-equal?
   (format-expected-min-lines
    `#(<> "abc\nghijkl" "def"))
   2)
  (check-equal?
   (format-expected-min-lines
    `#(<> "def" "abc\nghijkl"))
   2)
  (check-equal?
   (format-expected-min-lines
    `#(<> "abc\ndef" "ghi\njkl"))
   3)
  (check-equal?
   (format-expected-min-lines
    `#(<> "abc\ndef" #(<>) "ghi\njkl"))
   3)

  (check-equal?
   (format-expected-min-lines
    `#($$))
   0)

  (check-equal?
   (format-expected-min-lines
    `#($$ "abc\ndef"))
   2)
  (check-equal?
   (format-expected-min-lines
    `#($$ "abc" "def"))
   2)
  (check-equal?
   (format-expected-min-lines
    `#($$ "abc\nghijkl" "def"))
   3)
  (check-equal?
   (format-expected-min-lines
    `#($$ "def" "abc\nghijkl"))
   3)
  (check-equal?
   (format-expected-min-lines
    `#($$ "abc\ndef" "ghi\njkl"))
   4)
  (check-equal?
   (format-expected-min-lines
    `#($$ "abc\ndef" #(<>) "ghi\njkl"))
   5)
  )
(define (format-expected-min-lines pp-info)
  (match pp-info
    [(? string? s)
     (cond [(non-empty-string? s)
            (+ 1 (for/sum ([ch (in-string s)]
                           #:when (char=? ch #\newline))
                   1))]
           [else 0])]
    [`#(source ,source ,line ,col ,pos ,span)
     ;; TODO
     ((current-racket-format-print-source)
      (make-srcloc source
                   line
                   col
                   pos
                   span))]
    [`#(<> ,elements ...)
     (define min-lines
       (for/list ([element (in-list elements)])
         (format-expected-min-lines element)))
     (+ 1 (for/sum ([min-line (in-list min-lines)]
                    #:when (not (zero? min-line)))
            (- min-line 1)))]
    [`#($$ ,element0 ,elements ...)
     (define min-lines
       (for/list ([element (in-list (cons element0 elements))])
         (format-expected-min-lines element)))
     (for/sum ([min-line (in-list min-lines)])
       (cond [(zero? min-line) 1]
             [else             min-line]))]
    [`#($$)
     0]
    [`#(preserve-linebreak
        ,(and element0 `#(source ,source0 ,line0 ,col0 ,pos0 ,span0))
        ,(and elements `#(source ,source ,line ,col ,pos ,span)) ...)
     ;; TODO
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
     ;; TODO
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         (write-char #\space))
       (print-formatted element))]
    [`#(nest ,(? exact-integer? depth) ,element)
     ;; TODO get current column and set the format-indentation level
     (parameterize ([format-indentation (+ (format-indentation) depth)])
       (format-expected-min-lines element))]
    [`#(options ,name ,options ...)
     (define chosen-option (hash-ref (racket-format-config)
                                     name
                                     (lambda () (car (list-ref options 0)))))
     (define chosen-format (cdr (or (assoc chosen-option options)
                                    (list-ref options 0))))
     (format-expected-min-lines chosen-format)]))

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

#;(TODO of the sep combinator:

        #(sep ,element0 ,elements ...)
        (define min-lines
          (map format-expected-min-lines (cons element0 elements)))

        (cond
          [(> (apply max min-lines) 1)
           `#($$ ,element0 ,@elements)]
          [else
           (define max-column
             (+ (get-current-column)
                (for/sum ([...])
                  (format-expected-max-width element))))
           (cond
             [<= max-column (format-columns)])])
        )

(define (indent-at-current-col proc)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  (unless (and line col pos)
    (log-error "indent-at-current-col: port location is not available: ~a ~a ~a"
               line
               col
               pos))
  (parameterize ((format-indentation (or col (format-indentation))))
    (proc)))

(define (get-current-line-number)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  line)
