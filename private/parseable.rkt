#lang racket/base
(require racket/class)

(provide (all-from-out racket/class)
         parseable<%> get-pos set-pos pread ppeek
         parse-failed parse-failed?)

;;; Data Definition
(define parseable<%> (interface () get-pos set-pos read peek))

(define get-pos (generic parseable<%> get-pos))
(define set-pos (generic parseable<%> set-pos))
(define pread (generic parseable<%> read))
(define ppeek (generic parseable<%> peek))

;; Special Object
(define parse-failed (gensym "parse-failed"))

(define (parse-failed? x)
  (eq? parse-failed x))
