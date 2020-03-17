#lang racket/base
(require racket/class)

(provide (all-from-out racket/class)
         parseable<%> get-pos set-pos pread ppeek
         <EOF> parsable-name
         raise-parse-error (struct-out exn:parse-error)
         parse-failed parse-failed?)

;; Data Definition
;; 实现该接口时注意，get-pos应在源被parse完之后返回eof，基本parser应妥善处理eof。
(define parseable<%> (interface () get-pos set-pos read peek))

(define get-pos (generic parseable<%> get-pos))
(define set-pos (generic parseable<%> set-pos))
(define pread (generic parseable<%> read))
(define ppeek (generic parseable<%> peek))

;; Basic parser
(define (<EOF> in)
  (unless (eof-object? (send-generic in ppeek))
    parse-failed))

;; Special Object
(define parse-failed (gensym "parse-failed"))

(define (parse-failed? x)
  (eq? parse-failed x))

;; Pre-defined Error
(struct exn:parse-error exn:fail [])

(define (raise-parse-error msg)
  (raise (exn:parse-error msg (current-continuation-marks))))

;; Helper
(define parsable-name (make-parameter "Unknow"))
