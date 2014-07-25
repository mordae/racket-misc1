#lang racket/base
;
; Additional Events
;

(require racket/contract)

(require "syntax.rkt")

(provide
  (contract-out
    (alarm-in-evt (-> real? evt?))
    (timer-evt (-> real? (-> any) evt?))
    (recurring-evt (-> evt? procedure? evt?))))


(define (timer-evt interval handler)
  (replace-evt (alarm-in-evt interval)
               (λ_ (begin0 (timer-evt interval handler)
                           (handler)))))

(define (alarm-in-evt msecs)
  (let ((now (current-inexact-milliseconds)))
    (alarm-evt (+ now msecs))))

(define (recurring-evt base-evt handler)
  (producing new-evt
    (replace-evt base-evt (λ args
                            (apply handler args)
                            (values new-evt)))))


; vim:set ts=2 sw=2 et:
