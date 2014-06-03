#lang racket/base
;
; List Utilities
;

(require racket/contract
         racket/list)

(provide
  (all-from-out racket/list)
  (contract-out
    (list->values (-> list? any))
    (values* (->* () () #:rest list? any))
    (split-every (-> list? exact-positive-integer? (listof list?)))))


(define (list->values lst)
  (apply values lst))


(define (values* . value-or-list)
  (list->values
    (apply list* value-or-list)))


(define (split-every lst index)
  (let-values (((head tail) (split-at lst index)))
    (cons head (if (null? tail) null (split-every tail index)))))


; vim:set ts=2 sw=2 et:
