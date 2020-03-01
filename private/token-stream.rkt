#lang racket/base
(require "./parseable.rkt"
         racket/contract racket/struct
         (only-in racket/vector vector-copy)
         (for-syntax racket/base syntax/parse))

(provide (struct-out Token) define-tokens
         ignored ignored?
         (all-from-out "./parseable.rkt"))

;; Lexer
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lexer start-pos end-pos lexeme input-port file-path
         define-lex-abbrev define-lex-abbrevs define-lex-trans
         (struct-out position) (struct-out position-token)

         char-set any-char any-string nothing
         alphabetic lower-case upper-case title-case numeric
         symbolic punctuation graphic whitespace blank iso-control
         :* :+ :? := :>= :** :: :& :- :~ :/

         define-special-token
         (contract-out [do-lex (-> procedure? input-port? (is-a?/c parseable<%>))]))

(define (do-lex lexer in)
  (port-count-lines! in)
  (let loop ([toks '()])
    (if (eof-object? (peek-byte in))
        (new token-stream+c% [tokens (list->vector (reverse toks))])
        (let ([t (lexer in)])
          (cond
            [(ignored? t) (loop toks)]
            [else (loop (cons t toks))])))))

;; Token
(struct Token [name value start-pos end-pos]
  #:inspector (make-inspector)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda _ "Token")
      (lambda (t) (list (Token-name t)))))])

(define ((token-checker name) in)
  (let ([pos (send-generic in get-pos)]
        [x (send-generic in pread 1)])
    (cond
      [(and (Token? x) (eq? name (Token-name x))) x]
      [else (send-generic in set-pos pos) parse-failed])))

(define (new-position in)
  (let-values ([(l c p) (port-next-location in)])
    (position p l c)))

(define-syntax (define-special-token stx)
  (syntax-parse stx
    [(_ name:id body:expr ...+)
     (with-syntax ([(checker ...) (make-checker-names #'(name))])
       #'(begin
           (define (name in start)
             (define out (open-output-string))
             (parameterize ([current-input-port in]
                            [current-output-port out])
               (with-handlers ([string? (λ (s) (error 'name s))])
                 body ...)
               (Token 'name (get-output-string out) start (new-position in))))
           (define checker ... (token-checker 'name))))]))

(define-syntax (define-tokens stx)
  (syntax-parse stx
    [(_ name:id ...+)
     (with-syntax ([(checker ...) (make-checker-names #'(name ...))])
       #'(begin
           (begin
             (define-syntax (name stx)
               (syntax-parse stx
                 [(_) #'(Token 'name (void) start-pos end-pos)]
                 [(_ val:expr) #'(Token 'name val start-pos end-pos)]))
             (define checker (token-checker 'name)))
           ...))]))

;; Token Stream
(define token-stream%
  (class* object% (parseable<%>)
    (init-field [(toks tokens)])
    (super-new)

    (define pos 0)

    (define/public (get-pos)
      pos)

    (define/public (set-pos n)
      (set! pos n))

    (define/public (read [n 1])
      (define-values (l p) (values (vector-length toks) (+ pos n)))
      (cond
        [(>= pos l) eof]
        [(= n 1)
         (begin0
           (vector-ref toks pos)
           (set! pos p))]
        [else
         (begin0
           (vector-copy toks pos (min p l))
           (set! pos (min p l)))]))

    (define/public (peek [n 1])
      (define-values (l p) (values (vector-length toks) (+ pos n)))
      (cond
        [(>= pos l) eof]
        [(= n 1) (vector-ref toks pos)]
        [else (vector-copy toks pos (min p l))]))))

(define/contract token-stream+c%
  (class/c [get-pos (->m natural-number/c)]
           [set-pos (->m natural-number/c void?)]
           [read (->*m () (exact-positive-integer?) any)]
           [peek (->*m () (exact-positive-integer?) any)])
  token-stream%)

;; Special Object
(define ignored (gensym "ignored"))

(define (ignored? x)
  (eq? x ignored))

;; Helper

(begin-for-syntax
  (define (make-checker-names names)
    (let ([new-names
           (for/list ([ctx (in-list (syntax-e names))])
             (let ([n (string->symbol (format "<~a>" (syntax-e ctx)))])
               (datum->syntax ctx n ctx ctx)))])
      (datum->syntax #f new-names))))
