#lang racket/base
(require "../main.rkt")

;; Base
(define-lex-abbrevs
  (num1 (char-set "123456789"))
  (num0 (char-set "0123456789")))

(@ num (@u (@: <NUM> => (Token-value $0))
           (@: op0 <NUM>
               => (string->number (format "~a~a" $0 (Token-value $1))))))

(@ op0 (@: (@u <ADD> <SUB>) => (Token-value $0)))

(@ op1 (@: (@u <MUL> <DIV>) => (Token-value $0)))

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
(@ expr (@u op0-expr
            (@: op0-expr op0 expr => (list $1 $0 $2))))

(@ op0-expr (@u op1-expr
                (@: op1-expr op1 op0-expr => (list $1 $0 $2))))

(@ op1-expr (@u num
                (@: <L> expr <R> => $1)))

;; Test
(define ts
  (do-lex expr-lexer
          (open-input-string "(-2000 - +2 * (-100) + 100) + -300 * 200")))

(expr ts)
