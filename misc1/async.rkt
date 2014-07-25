#lang racket/base
;
; Asynchronous Tasks
;

(require racket/contract)

(require "syntax.rkt")

(provide async)

(provide
  (contract-out
    (async-task? predicate/c)
    (async-task-send (-> async-task? any/c void?))
    (rename async-proc async-task (-> (-> any) async-task?))))


(struct success
  (values))

(struct failure
  (exception))

(struct async-task
  (evt thread)
  #:property prop:evt (struct-field-index evt))


(define (async-proc proc)
  (let* ((result #f)
         (thread (thread (λ_ (set! result (call/wrap proc))))))
    (async-task (wrap-evt thread (λ_ (unwrap result))) thread)))

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

(define (async-task-send at v)
  (let ((thread (async-task-thread at)))
    (thread-send thread v)))


; vim:set ts=2 sw=2 et:
