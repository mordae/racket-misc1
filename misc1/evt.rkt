#lang racket/base
;
; Additional Events
;

(require racket/contract)

(require "syntax.rkt")

(provide
  (contract-out
    (alarm-in-evt (-> real? evt?))
    (constant-evt (->* () () #:rest list? evt?))
    (cache-evt (-> evt? evt?))

    (trigger-evt? predicate/c)
    (make-trigger-evt (-> trigger-evt?))
    (trigger! (->* (trigger-evt?) () #:rest list? void?))
    (cancel! (-> trigger-evt? void?))

    (epoch-evt? predicate/c)
    (make-epoch-evt (-> epoch-evt?))
    (epoch-evt-advance! (->* (epoch-evt?) () #:rest list? void?))))


(struct trigger-evt
  (lock (results #:mutable) evt)
  #:property prop:evt (struct-field-index evt))

(struct epoch-evt
  ((trigger-evt #:mutable) evt)
  #:property prop:evt (struct-field-index evt))


(when-defined replace-evt
  (provide
    (contract-out
      (timer-evt (-> real? (-> any) evt?))
      (recurring-evt (->* (evt?) (procedure?) evt?))))

  (define (timer-evt interval handler)
    (let ((alarm (alarm-in-evt 0)))
      (recursive (new-evt)
        (guard-evt
          (Λ (replace-evt alarm
                          (λ _
                            (set! alarm (alarm-in-evt interval))
                            (begin0 new-evt (handler)))))))))

  (define (recurring-evt base-evt (handler void))
    (recursive (new-evt)
      (replace-evt base-evt (λ args
                              (apply handler args)
                              (values new-evt))))))

(define (alarm-in-evt msecs)
  (let ((now (current-inexact-milliseconds)))
    (alarm-evt (+ now msecs))))

(define (constant-evt . args)
  (wrap-evt always-evt (λ _ (apply values args))))

(define (cache-evt evt)
  (letrec ((new-evt (wrap-evt evt (λ args
                                    (set! new-evt (apply constant-evt args))
                                    (apply values args)))))
    (guard-evt (Λ new-evt))))

(define (make-trigger-evt)
  (recursive (self)
    (let ((lock (make-semaphore 0)))
      (trigger-evt lock #f
                   (wrap-evt (semaphore-peek-evt lock)
                             (λ (semaphore)
                               (apply values (trigger-evt-results self))))))))

(define (trigger! evt . args)
  (let ((lock (trigger-evt-lock evt)))
    ;; Ensure that the trigger is locked.
    (semaphore-try-wait? lock)

    ;; Set new result values.
    (set-trigger-evt-results! evt args)

    ;; Open the trigger.
    (semaphore-post lock)))

(define (cancel! evt)
  ;; Ensure that the trigger is locked.
  (semaphore-try-wait? (trigger-evt-lock evt))

  ;; Free old result values.
  (set-trigger-evt-results! evt #f))

(define (make-epoch-evt)
  (recursive (self)
    (epoch-evt (make-trigger-evt)
               (guard-evt (Λ (epoch-evt-trigger-evt self))))))

(define (epoch-evt-advance! evt . args)
  (let ((old-trigger-evt (epoch-evt-trigger-evt evt)))
    (set-epoch-evt-trigger-evt! evt (make-trigger-evt))
    (apply trigger! old-trigger-evt args)))


; vim:set ts=2 sw=2 et:
