#lang racket/base
;
; List Utilities
;

(require racket/contract
         racket/list)

(provide
  (contract-out
    (list->values (-> list? any))
    (values* (->* () () #:rest list? any))
    (split-every (-> list? exact-positive-integer? (listof list?)))))

(provide let-list)


(define (list->values lst)
  (apply values lst))


(define (values* . value-or-list)
  (list->values
    (apply list* value-or-list)))


(define (split-every lst index)
  (let-values (((head tail) (split-at lst index)))
    (cons head (if (null? tail) null (split-every tail index)))))


;; Similar to let-values, but instead of multiple return values
;; consuming lists.
(define-syntax-rule (let-list (((name ...) value) ...) body ...)
  (let-values (((name ...) (list->values value)) ...)
    (begin body ...)))


; vim:set ts=2 sw=2 et:
