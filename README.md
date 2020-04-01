silver-parser
=============
## A Simple Example
Defining an expression parser. You can find a complete example at [here][1].

### Lexer
Use macro `lexer` to define port-based lexer. It is based on `parser-tools/lex`.
```Racket
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
```

### Parser
Use macro `@` to define token-based parser. Parser is just a function when receive an `parsable-stream<%>` and return the parsed result or `parse-failed`.

```Racket
;; Parser
(@ expr (@U op0-expr
            (@: #:msg "bad op0 expr"
                op0-expr op0 ! expr => (list $1 $0 $2))))

(@ op0-expr (@U op1-expr
                (@: #:msg "bad op1 expr"
                    op1-expr op1 ! op0-expr => (list $1 $0 $2))))

(@ op1-expr (@U num
                (@: #:msg "can't find match bracket"
                    <L> ! expr <R> => $1)))
```

Parser defined with `@` will have left-recursion-detection automatically. You can use parser as a normal function.

```Racket
(define ts
  (do-lex expr-lexer
          (open-input-string "(-2000 - +2 * (-100 + 100) + -1) -300 * 200")))

(expr ts)
;;=>
;;(E
;; '+
;; (E
;;  '-
;;  (E '- (P '- (P '- 1)) (P '+ (P '+ 2)))
;;  (E '/ (E '* 3 (P '- 4)) (P '+ (P '+ 5))))
;; (E '+ (E '/ 6 3) 2))
```

[1]:	https://github.com/SFurnace/cb-compiler/blob/master/private/parser.rkt