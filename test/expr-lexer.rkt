#lang racket/base
(require "../private/token-stream.rkt")
  

(define-lex-abbrevs
  (num1 (char-set "123456789"))
  (num0 (char-set "0123456789")))

(define expr-lexer
  (lexer
   [(:+ whitespace) ignore]
   [(union "+" "-" "*" "/") (Tok A (string->symbol lexeme))]
   ["(" (Tok L)]
   [")" (Tok R)]
   [(union "0"
           (:: num1 (:* num0))
           (:: num1 (:* num0) "." (:+ num0)))
    (Tok N (string->number lexeme))]))

(define ts (do-lex expr-lexer (open-input-string "((100) + 100) * 200")))
