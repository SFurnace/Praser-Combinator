#lang racket/base
(require "./parseable.rkt"
         racket/set racket/contract
         (for-syntax racket/base racket/list syntax/parse))

(provide (all-from-out "./parseable.rkt")
         @ @* @+ @? @= @>= @** @u @:)

;; Parser ::= parseable<%> -> Any | parse-failed
;; 若parse不成功，则parseable<%>的pos需要保持不变
(define left-recursion-records (make-parameter (set)))

(define-syntax (@ stx)
  (syntax-parse stx
    [(_ name:id parser:expr)
     #'(define (name in)
         (let* ([pos (send-generic in get-pos)]
                [mark (cons name pos)]
                [records (left-recursion-records)])
           (if (set-member? records mark)
               parse-failed
               (parameterize ([left-recursion-records (set-add records mark)])
                 (parser in)))))]))

;; Repeat Combinators
(define (((@repeat m n) parser) in)
  (define pos (send-generic in get-pos))
  (let loop ([rs '()])
    (let ([r (parser in)])
      (if (parse-failed? r)
          (if (<= m (length rs) n)
              (reverse rs)
              (begin
                (send-generic in set-pos pos)
                parse-failed))
          (loop (cons r rs))))))

(define @* (@repeat 0 +inf.0))
(define @+ (@repeat 1 +inf.0))
(define @? (@repeat 0 1))
(define/contract (@= n)
  (-> natural-number/c any)
  (@repeat n n))
(define/contract (@>= n)
  (-> natural-number/c any)
  (@repeat n +inf.0))
(define/contract (@** m n)
  (-> natural-number/c natural-number/c any)
  (@repeat m n))

;; Choice Combinators
(define ((@u . parsers) in)
  (define pos (send-generic in get-pos))
  (define rs
    (filter (lambda (x) (not (parse-failed? (car x))))
            (for/list ([p (in-list parsers)])
              (send-generic in set-pos pos)
              (cons (p in) (send-generic in get-pos)))))
  (if (null? rs)
      (begin
        (send-generic in set-pos pos)
        parse-failed)
      (let ([r (car (sort rs > #:key cdr))])
        (send-generic in set-pos (cdr r))
        (car r))))

;; Sequence Combinators
(define-syntax (@: stx)
  (syntax-parse stx #:datum-literals (=>)
    [(_ exp:expr ...+ => body:expr ...+)
     (with-syntax ([(name ...) (make-temp-names #'(exp ...))])
       #'(lambda (in)
           (let/ec k
             (let* ([pos (send-generic in get-pos)]
                    [fail (λ () (send-generic in set-pos pos) (k parse-failed))]
                    [name (let ([v exp])
                            (if (procedure? v)
                                (let ([r (v in)])
                                  (if (parse-failed? r)
                                      (fail)
                                      r))
                                v))]
                    ...)
               body ...))))]
    [(_ exp:expr ...+)
     #`(@: exp ... => (list #,@(make-temp-names #'(exp ...))))]))

;; Other Patterns

;; Helper
(begin-for-syntax
  (define (make-temp-names stx)
    (let ([temps (for/list ([n (in-naturals)]
                            [s (in-list (syntax-e stx))])
                   (datum->syntax s (string->symbol (format "$~a" n)) s s))])
      (datum->syntax #f temps))))
