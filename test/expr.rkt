#lang racket/base
(require "../main.rkt")

;; Base
(define-lex-abbrevs
  (num1 (char-set "123456789"))
  (num0 (char-set "0123456789")))

;; Lexer
(define-tokens ADD SUB MUL DIV L R NUM)

(define expr-lexer
  (lexer
   [(:+ whitespace) ignored]
   ["+" (ADD '+)]
   ["-" (SUB '-)]
   ["*" (MUL '*)]
   ["/" (DIV '/)]
   ["(" (L)]
   [")" (R)]
   [(union "0"
           (:: num1 (:* num0))
           (:: num1 (:* num0) "." (:+ num0)))
    (NUM (string->number lexeme))]))

;; Parser
(@ num (@: <NUM> => (Token-value $0)))

(@ op0 (@: (@u <ADD> <SUB>) => (Token-value $0)))

(@ op1 (@: (@u <MUL> <DIV>) => (Token-value $0)))

(@ expr additive)

(@ additive (@u (@infix-left op0 multiplicative #:constructor E)
                multiplicative))

(@ multiplicative (@u (@infix-left op1 prefix #:constructor E)
                      prefix))

(@ prefix (@u (@prefix op0 unary #:constructor P)
              unary))

(@ unary (@u (@: <L> expr <R> => $1)
             num))

;; Test
(struct E [op l r] #:transparent)
(struct P [op e] #:transparent)

(define t0 "(-2000 - +2 * (-100 + 100) + -1) - 300 * 200")

(define ts
  (do-lex expr-lexer
          (open-input-string "--1 - ++2 - 3 * (-4) / ++5 + (6 / 3 + 2)")))

(expr ts)