#lang racket/base
(require "../main.rkt")

(define-lex-abbrevs
  (num1 (char-set "123456789"))
  (num0 (char-set "0123456789")))

(define-tokens op-0 op-1 l r num)

(define expr-lexer
  (lexer
   [(:+ whitespace) ignored]
   [(union "+" "-") (op-0 (string->symbol lexeme))]
   [(union "*" "/") (op-1 (string->symbol lexeme))]
   ["(" (l)]
   [")" (r)]
   [(union "0"
           (:: num1 (:* num0))
           (:: num1 (:* num0) "." (:+ num0)))
    (num (string->number lexeme))]))


(module+ test
  (define ts
    (do-lex expr-lexer
            (open-input-string "(2000 - 2 * (100) + 100) + 300 * 200")))

  (@ expr
     (@u op0-expr
         (@: op0-expr <op-0> expr => (list (Token-value $1) $0 $2))))

  (@ op0-expr
     (@u op1-expr
         (@: op1-expr <op-1> op0-expr => (list (Token-value $1) $0 $2))))

  (@ op1-expr
     (@u (@: <num> => (Token-value $0))
         (@: <l> expr <r> => $1)))

  (expr ts)

  )
