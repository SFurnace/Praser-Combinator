#lang racket/base
(require racket/class)
(provide (all-from-out racket/class)
         (all-defined-out))

;;; Data Definition
(define parseable<%> (interface () get-pos set-pos read peek))

(define get-pos (generic parseable<%> get-pos))
(define set-pos (generic parseable<%> set-pos))
(define pread (generic parseable<%> read))
(define ppeek (generic parseable<%> peek))

;; Special Object
(define parse-failed
  (let ()
    (struct FAILED []
      #:inspector (make-inspector)
      #:methods gen:custom-write
      [(define (write-proc s in _)
         (display "FAILED" in))])
    (FAILED)))
