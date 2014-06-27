#lang racket/base
;
; Structure representing an unsuccessfull result of a procedure
; that is to be expected and needs to be properly handled.
;

(require racket/contract
         racket/match)

(provide
  (all-defined-out))


(struct reject
  (info data)
  #:guard (λ (info data name)
             (unless (string? info)
               (raise-argument-error 'reject "string?" 0 info data))
             (values info data))
  #:transparent)


; vim:set ts=2 sw=2 et:
