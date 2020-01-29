#lang racket/base
(require racket/class)

(provide (all-from-out racket/class)
         parseable<%> get-pos set-pos pread ppeek
         parse-failed parse-failed?)

;; Data Definition
;; 实现该接口时注意，get-pos应在源被parse完之后返回eof，基本parser应妥善处理eof。
(define parseable<%> (interface () get-pos set-pos read peek))

(define get-pos (generic parseable<%> get-pos))
(define set-pos (generic parseable<%> set-pos))
(define pread (generic parseable<%> read))
(define ppeek (generic parseable<%> peek))

;; Special Object
(define parse-failed (gensym "parse-failed"))

(define (parse-failed? x)
  (eq? parse-failed x))
