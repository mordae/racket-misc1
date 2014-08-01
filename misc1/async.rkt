#lang racket/base
;
; Asynchronous Tasks
;

(require racket/contract)

(require "syntax.rkt"
         "evt.rkt")

(provide async
         async/loop)

(provide
  (contract-out
    (async-task? predicate/c)
    (async-task-send (-> async-task? any/c void?))
    (rename async-proc async-task (-> (-> any) async-task?))
    (rename async-loop-proc async-task/loop (-> (-> any) async-task?))))


(struct success
  (values))

(struct failure
  (exception))

(struct async-task
  (thread evt)
  #:property prop:evt (struct-field-index evt))


(define (async-proc proc)
  (let* ((channel (make-channel))
         (thread (thread (λ ()
                           (channel-put channel (call/wrap proc))))))
    (async-task thread
                (wrap-evt (cache-evt channel)
                          (λ (result)
                            (unwrap result))))))

(define (async-loop-proc proc)
  (let* ((channel (make-channel))
         (thread (thread (λ ()
                           (loop
                             (channel-put channel (call/wrap proc)))))))
    (async-task thread
                (wrap-evt channel
                          (λ (result)
                            (unwrap result))))))

(define (call/wrap proc)
  (with-handlers ((void (λ (exn)
                          (failure exn))))
    (call-with-values proc (λ results
                             (success results)))))

(define (unwrap result)
  (if (success? result)
      (apply values (success-values result))
      (raise (failure-exception result))))

(define (async-task-send at v)
  (let ((thread (async-task-thread at)))
    (thread-send thread v)))


(define-syntax-rule (async body ...)
  (async-proc
    (λ () body ...)))

(define-syntax-rule (async/loop body ...)
  (async-loop-proc
    (λ () body ...)))


; vim:set ts=2 sw=2 et:
