#lang racket/base

(require racket/match
         racket/file
         racket/port
         (for-syntax racket/base
                     racket/sequence
                     syntax/parse))

(provide (all-defined-out))

(begin-for-syntax
  (define (attach-name stx [base "g"])
    (define name (gensym base))
    (values (syntax-property stx 'syncheck:format:name name)
            name))
  )

;; FIXME handle `else` and properly format it because
;; it is moved into the 'disappeared-use property
(define-syntax (my-cond stx)
  (syntax-parse stx
    #:literals (else)
    [(form (expr ...+) ...)
     (define-values (exprss/name namess)
       (for/lists (exprss/name namess)
                  ([exprs (in-syntax #'((expr ...) ...))])
         (for/lists (exprs/name names)
                    ([expr (in-syntax exprs)])
           (attach-name expr "my-cond.clause"))))
     (with-syntax ([((expr ...) ...) exprss/name])
       (syntax-property
        (syntax/loc stx (cond [expr ...] ...))
        'syncheck:format
        (let ([body `#($$ ,@(for/list ([names (in-list namess)])
                             (define names-with-spaces (apply append (for/list ([name (in-list names)])
                                                                      (list " " name))))
                             `#(<> "[" #(options
                                         'cond-body-line-break-formatting-rules
                                         ,(cons 'default
                                               `#(preserve-linebreak ,@names))
                                         ,(cons 'same-line
                                               `#(<> ,@(if (pair? names-with-spaces)
                                                          (cdr names-with-spaces)
                                                          names-with-spaces)))
                                         ,(cons 'force-line-break
                                               `#($$ ,@names)))
                                   "]")))])
          `#(<> "("
                #(options
                  'cond-first-clause-formatting-rules
                  ,(cons 'the-first-clause-follows-the-cond
                        `#(<> ,(symbol->string (syntax-e #'form)) " " ,body))
                  ,(cons 'the-first-clause-in-a-new-line
                        `#($$ ,(symbol->string (syntax-e #'form)) #(nest 1 ,body))))
                ")"))))]))

#|
(pretty-write
 #(let ([body
         #($$ #(<> "["
                   #(options
                     'cond-body-line-break-formatting-rules
                     (cons 'default
                           #(preserve-linebreak #(source) #(source)))
                     (cons 'same-line
                           #(<> #(source) " " #(source)))
                     (cons 'force-line-break
                           #($$ #(source)
                                #(source))))
                   "]")
              ...)
         ])
    #(<>
      "("
      #(options
        'cond-first-clause-formatting-rules
        (cons 'the-first-clause-follows-the-cond
              #(<> "my-cond" " " body))
        (cons 'the-first-clause-in-a-new-line
              #($$ "my-cond" #(nest 1 body)))
      ")"))
 ))
|#


#|
    |"(" "let" " (" |"[" a 10 ... "]"
                    |"[" ..... "]" ")"

    |__ body.... ")"
|#


#|
    "("|"let" " (" |"[" a 10 ... "]"
                   |"[" ..... "]" ")"
       |_body....         ")"
|#

(define-syntax (my-let stx)
  (syntax-parse stx
    [(form ([lhs rhs] ...) body-expr ...+)
     (define-values (exprss/name namess)
       (for/lists (exprss/name namess)
                  ([exprs (in-syntax #'((lhs rhs) ...))])
         (for/lists (exprs/name names)
                    ([expr (in-syntax exprs)])
           (attach-name expr "my-let.clause"))))
     (define-values (body-exprss/name body-namess)
       (for/lists (body-exprss/name body-namess)
                  ([body (in-syntax #'(body-expr ...))])
         (attach-name body "my-let-body.clause")))
     (with-syntax ([((lhs rhs) ...) exprss/name]
                   [(body-expr ...) body-exprss/name])
       (syntax-property
        (syntax/loc stx (let ([lhs rhs] ...) body-expr ...))
        'syncheck:format
        `#(<> "("
              #($$ #(<> ,(symbol->string (syntax-e #'form))
                        " ("
                        #($$ ,@(for/list ([names (in-list namess)])
                                 `#(<> "["
                                       ,(list-ref names 0)
                                       " "
                                       ,(list-ref names 1)
                                       "]")))
                        ")")
                   #(nest 1
                          #($$ ,@(for/list ([body-name (in-list body-namess)])
                                   body-name))))
              ")")))]))
     
(define (extract-name-syntax-maps stx
                                  #:check-disappeared-use? [check-disappeared-use? #f])
  (define table (make-hash))
  (define (do-traverse stx)
    (define name (syntax-property stx 'syncheck:format:name))
    (when name
      (hash-set! table name stx))
    (define datum (syntax-e stx))
    (cond
      [(list? datum)
       (for ([substx (in-list datum)])
         (do-traverse substx))]
      [(vector? datum)
       (for ([substx (in-vector datum)])
         (do-traverse substx))]
      [(pair? datum)
       (do-traverse (car datum))
       (do-traverse (cdr datum))]
      ;; TODO recursively traverse all compound data
      [else (void)]))

  (do-traverse stx)
  (when check-disappeared-use?
    (define uses (syntax-property stx 'disappeared-use))
    (let loop ([use uses])
      (cond
        [(syntax? use)
         (do-traverse use)]
        [(pair? use)
         (loop (car use))
         (loop (cdr use))]
        [else (void)])))
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

(define (construct-formatting-info stx)
  (define expanded-stx (expand stx))
  (define name-stx-map
    (extract-name-syntax-maps
     expanded-stx
     #:check-disappeared-use? #t))
  (construct-formatting-info-from-syntax name-stx-map expanded-stx))

(define (racket-format stx)
  (with-output-to-string
    (λ ()
      (port-count-lines! (current-output-port))
      (print-formatted
       (construct-formatting-info stx)))))
