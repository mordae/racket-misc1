#lang racket/base
;
; Asynchronous Tasks
;

(require racket/contract)

(provide async)

(require "syntax.rkt")


(struct success
  (values))

(struct failure
  (exception))


(provide
  (contract-out
    (async-proc (-> (-> any) evt?))))


(define (async-proc proc)
  (let ((cached-result #f))
    (guard-evt (λ ()
                 (cond
                   (cached-result
                     (wrap-evt always-evt
                               (λ_ (unwrap cached-result))))

                   (else
                     (wrap-evt (producing (ch (make-channel))
                                 (thread (λ_ (channel-put ch (call/wrap proc)))))
                               (λ (result)
                                 (producing (value (unwrap result))
                                   (set! cached-result result))))))))))


(define (call/wrap proc)
  (with-handlers ((void (λ (exn)
                          (failure exn))))
    (call-with-values proc (λ results
                             (success results)))))


(define (unwrap result)
  (if (success? result)
      (apply values (success-values result))
      (raise (failure-exception result))))


(define-syntax-rule (async body ...)
  (async-proc
    (λ () body ...)))


; vim:set ts=2 sw=2 et:
