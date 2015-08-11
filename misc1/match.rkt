#lang racket/base
;
; Additional Match Expanders
;

(require racket/match)

(require
  (for-syntax racket/base
              syntax/parse))

(provide hash-lookup)


(define-match-expander hash-lookup
  (syntax-parser
    ((_ (k v) ...)
     #`(and (? hash?)
            (and #,@#'((app (λ (ht)
                              (and (hash-has-key? ht k)
                                   (box (hash-ref ht k))))
                            (? values (app unbox v))) ...))))))


; vim:set ts=2 sw=2 et:
