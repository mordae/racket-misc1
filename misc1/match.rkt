#lang racket/base
;
; Match Extensions
;

(require racket/match)

(require
  (for-syntax racket/base))

(provide equal)


(define-match-expander equal
  (syntax-rules (equal)
    ((_ muster)
     (? (λ (value)
          (equal? value muster))))))


; vim:set ts=2 sw=2 et:
