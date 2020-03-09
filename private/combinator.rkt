#lang racket/base
(require "./parseable.rkt"
         racket/set racket/contract
         (for-syntax racket/base racket/list syntax/parse))

(provide (all-from-out "./parseable.rkt")
         @ @: @* @*? @+ @+? @? @?? @= @>= @>=? @** @**? @u @U @may @try @sepBy
         @infix-left @infix-right @prefix @postfix)

;; Parser ::= parseable<%> -> Any
;; 若parse不成功，则parseable<%>的pos需要保持不变

;; parameters
(define left-recursion-records (make-parameter (set)))
(define trace-back-records (make-parameter '()))

;; parser binder
(define-syntax (@ stx)
  (syntax-parse stx
    [(_ name:id parser:expr)
     #'(define (name in)
         (let* ([pos (send-generic in get-pos)]
                [mark (cons name pos)]
                [records (left-recursion-records)])
           (if (set-member? records mark)
               parse-failed
               (parameterize ([left-recursion-records (set-add records mark)]
                              [trace-back-records (trace-back-records)])
                 (parser in)))))]))

;; Sequence Combinators
(define-syntax (@: stx)
  (syntax-parse stx #:datum-literals (! =>)
    [(_ (~optional (~seq #:msg msg:str) #:defaults ([msg #'"parse failed"]))
        e0:expr ...+ ! e1:expr ...+  => body:expr ...+)
     (let* ([names0 (make-temp-names #'(e0 ...))]
            [len (length (syntax->list names0))]
            [names1 (make-temp-names #'(e1 ...) len)])
       (with-syntax ([(n0 ...) names0]
                     [(n1 ...) names1])
         #'(lambda (in)
             (let/ec k
               (let* ([pos (send-generic in get-pos)]
                      [fail (λ () (send-generic in set-pos pos) (k parse-failed))]
                      [n0 (let ([r (e0 in)])
                            (if (parse-failed? r)
                                (fail)
                                r))]
                      ...
                      [n1 (let ([r (e1 in)])
                            (if (parse-failed? r)
                                (raise-parse-error msg)
                                r))]
                      ...)
                 body ...)))))]
    [(_ exp:expr ...+ => body:expr ...+)
     (with-syntax ([(name ...) (make-temp-names #'(exp ...))])
       #'(lambda (in)
           (let/ec k
             (let* ([pos (send-generic in get-pos)]
                    [fail (λ () (send-generic in set-pos pos) (k parse-failed))]
                    [name (let ([r (exp in)])
                            (if (parse-failed? r)
                                (fail)
                                r))]
                    ...)
               body ...))))]
    [(_ e0:expr ...+ ! e1:expr ...+)
     #`(@: e0 ... ! e1 ... => (list #,@(make-temp-names #'(e0 ... e1 ...))))]
    [(_ exp:expr ...+)
     #`(@: exp ... => (list #,@(make-temp-names #'(exp ...))))]))

;; Repeat Combinators
(define (((@repeat m n) parser) in)
  (define pos (send-generic in get-pos))
  (define (fail)
    (send-generic in set-pos pos)
    parse-failed)
  (let loop ([rs '()])
    (if (= (length rs) n)
        (reverse rs)
        (let ([r (parser in)])
          (if (parse-failed? r)
              (if (<= m (length rs) n)
                  (reverse rs)
                  (fail))
              (loop (cons r rs)))))))

(define (((@repeat? m n) parser #:successor [s #f]) in)
  (define succ (if s (@may s) #f))
  (define pos (send-generic in get-pos))
  (define (fail)
    (send-generic in set-pos pos)
    parse-failed)
  (let loop ([rs '()])
    (if (> (length rs) n)
        (fail)
        (if (and (<= m (length rs)) (if succ (succ in) #t))
            (reverse rs)
            (let ([r (parser in)])
              (if (parse-failed? r)
                  (fail)
                  (loop (cons r rs))))))))

(define @* (@repeat 0 +inf.0))
(define @*? (@repeat? 0 +inf.0))
(define @+ (@repeat 1 +inf.0))
(define @+? (@repeat? 1 +inf.0))
(define @? (@repeat 0 1))
(define @?? (@repeat? 0 1))
(define/contract (@= n)
  (-> natural-number/c any)
  (@repeat n n))
(define/contract (@>= n)
  (-> natural-number/c any)
  (@repeat n +inf.0))
(define/contract (@>=? n)
  (-> natural-number/c any)
  (@repeat? n +inf.0))
(define/contract (@** m n)
  (-> natural-number/c natural-number/c any)
  (@repeat m n))
(define/contract (@**? m n)
  (-> natural-number/c natural-number/c any)
  (@repeat? m n))

;; Choice Combinators
(define ((@u . parsers) in)
  (define pos (send-generic in get-pos))
  (let loop ([ps parsers])
    (if (null? ps)
        (begin
          (send-generic in set-pos pos)
          parse-failed)
        (let* ([p (car ps)]
               [r (p in)])
          (if (parse-failed? r)
              (loop (cdr ps))
              r)))))

(define ((@U . parsers) in)
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

;; Other Patterns
(define ((@may p) in)
  (let ([c (send-generic in get-pos)])
    (dynamic-wind
     (λ () '_)
     (λ () (if (parse-failed? (p in)) #f #t))
     (λ () (send-generic in set-pos c)))))

(define ((@try p) in)
  (let ([c (send-generic in get-pos)]
        [r (p in)])
    (if (parse-failed? r)
        (begin
          (send-generic in set-pos c)
          #f)
        r)))

(define (@sepBy p0 p1)
  (@* (@: p1 (@try p0) => $0)))

(define ((@infix op elem associativity #:constructor [c list]) in)
  (define pos0 (send-generic in get-pos))
  (define (fail)
    (send-generic in set-pos pos0)
    parse-failed)
  (define (construct lst)
    (let ([lst (if (eq? associativity 'right) lst (reverse lst))])
      (let loop ([l (cdr lst)]
                 [e0 (car lst)])
        (let*  ([op (car l)]
                [e1 (cadr l)]
                [l (cddr l)]
                [exp (if (eq? associativity 'right) (c op e1 e0) (c op e0 e1))])
          (if (null? l)
              exp
              (loop l exp))))))
  (define (loop0 stk pos)
    (let ([v (elem in)])
      (if (parse-failed? v)
          (if (> (length stk) 3)
              (begin
                (send-generic in set-pos pos)
                (construct (cdr stk)))
              (fail))
          (if (< (length stk) 2)
              (loop1 (cons v stk) pos)
              (loop1 (cons v stk) (send-generic in get-pos))))))
  (define (loop1 stk pos)
    (let ([v (op in)])
      (if (parse-failed? v)
          (if (>= (length stk) 3)
              (construct stk)
              (fail))
          (loop0 (cons v stk) pos))))

  (loop0 '() pos0))

(define (@infix-left op elem #:constructor [c list])
  (@infix op elem 'left #:constructor c))

(define (@infix-right op elem #:constructor [c list])
  (@infix op elem 'right #:constructor c))

(define (@prefix op elem #:constructor [c list])
  (@: (@+ op) elem => (foldl c $1 $0)))

(define (@postfix op elem #:constructor [c list])
  (@: elem (@+ op) => (foldl c $0 $1)))

;; Helper
(begin-for-syntax
  (define (make-temp-names stx [start 0])
    (let ([temps (for/list ([n (in-naturals start)]
                            [s (in-list (syntax-e stx))])
                   (datum->syntax s (string->symbol (format "$~a" n)) s s))])
      (datum->syntax #f temps))))
