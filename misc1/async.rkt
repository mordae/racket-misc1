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
  (let ((result #f))
    (wrap-evt (thread (λ_ (set! result (call/wrap proc))))
              (λ_ (unwrap result)))))


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
