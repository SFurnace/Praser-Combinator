#lang racket/base
(require "./parseable.rkt"
         racket/contract
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (only-in racket/vector vector-copy)
         (for-syntax racket/base syntax/parse))

(provide Tok (struct-out Token) <raw-tok> <tok> ignore
         (contract-out [do-lex (-> procedure? input-port? (is-a?/c parseable<%>))])

         lexer start-pos end-pos lexeme input-port file-path
         define-lex-abbrev define-lex-abbrevs define-lex-trans

         char-set any-char any-string nothing
         alphabetic lower-case upper-case title-case numeric
         symbolic punctuation graphic whitespace blank iso-control
         :* :+ :? := :>= :** :: :& :- :~ :/

         (all-from-out "./parseable.rkt"))

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
         (begin0 (vector-ref toks pos)
                 (set! pos p))]
        [else
         (begin0 (vector-copy toks pos (min p l))
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

(define ignore
  (let ()
    (struct IGNORE []
      #:inspector (make-inspector)
      #:methods gen:custom-write
      [(define (write-proc s in _)
         (display "IGNORE" in))])
    (IGNORE)))

(define (do-lex lexer in)
  (port-count-lines! in)
  (let loop ([toks '()])
    (if (eof-object? (peek-byte in))
        (new token-stream+c% [tokens (list->vector (reverse toks))])
        (let ([t (lexer in)])
          (cond
            [(eq? ignore t) (loop toks)]
            [else (loop (cons t toks))])))))

;; Token
(struct Token [name value start-pos end-pos]
  #:inspector (make-inspector)
  #:methods gen:custom-write
  [(define (write-proc s out _)
     (fprintf out "#<Token ~a>" (Token-name s)))])

(define-syntax (Tok stx)
  (syntax-parse stx
    [(_ name:id) #'(Token 'name (void) start-pos end-pos)]
    [(_ name:id val:expr) #'(Token 'name val start-pos end-pos)]))

(define ((token-parser name [raw #f]) in)
  (let ([pos (send-generic in get-pos)]
        [x (send-generic in pread 1)])
    (cond
      [(and (Token? x) (eq? name (Token-name x))) x]
      [else (send-generic in set-pos pos) parse-failed])))

(define-syntax (<tok> stx)
  (syntax-parse stx
    [(_ name:id) #'(token-parser 'name #f)]))

(define-syntax (<raw-tok> stx)
  (syntax-parse stx
    [(_ name:id) #'(token-parser 'name #t)]))

