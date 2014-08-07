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
    (recurring-evt (->* (evt?) (procedure?) evt?))
    (constant-evt (->* () () #:rest list? evt?))
    (cache-evt (-> evt? evt?))))


(define (timer-evt interval handler)
  (let ((alarm (alarm-in-evt 0)))
    (producing new-evt
      (guard-evt
        (Λ (replace-evt alarm
                        (λ _
                          (set! alarm (alarm-in-evt interval))
                          (begin0 new-evt (handler)))))))))

(define (alarm-in-evt msecs)
  (let ((now (current-inexact-milliseconds)))
    (alarm-evt (+ now msecs))))

(define (recurring-evt base-evt (handler void))
  (producing new-evt
    (replace-evt base-evt (λ args
                            (apply handler args)
                            (values new-evt)))))

(define (constant-evt . args)
  (wrap-evt always-evt (λ _ (apply values args))))

(define (cache-evt evt)
  (let ([result #f])
    (guard-evt (Λ (if result
                      (apply constant-evt result)
                      (wrap-evt evt (λ args
                                      (set! result args)
                                      (apply values args))))))))


; vim:set ts=2 sw=2 et:
