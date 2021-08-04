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
(define format-write-char (make-parameter write-char))
(define format-write-string (make-parameter write-string))

(define (print-formatted-newline)
  ((format-write-char) #\newline)
  (for ([i (in-range (format-indentation))])
    ((format-write-char) #\space)))

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
        (parameterize ([current-error-port (open-output-nowhere)])
          (print-formatted-newline)))
      ((format-write-string) (substring a-line space-count-at-start)))))

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
     ((format-write-string) s)]
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
             ((format-write-char) #\space)])
          (define current-line-no (get-current-line-number))
          (print-formatted element)
          (- (get-current-line-number) current-line-no))))]
    [`#(preserve-linebreak ,elements ...)
     (for ([(element idx) (in-indexed elements)])
       (when (> idx 0)
         ((format-write-char) #\space))
       (print-formatted element))]
    [`#(nest ,(? exact-integer? depth) ,element)
     (for ([i (in-range depth)])
       ((format-write-char) #\space))
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

(define (get-current-col)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  col)

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
