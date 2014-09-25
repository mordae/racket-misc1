#lang racket/base
;
; Faster Asynchronous Channel
;

(require racket/contract)

(require "syntax.rkt")

(provide
  (contract-out
    (fast-channel? predicate/c)
    (make-fast-channel (-> fast-channel?))
    (fast-channel-put (->* (fast-channel?) () #:rest list? void?))
    (fast-channel-get (-> fast-channel? any))
    (fast-channel-try-get (-> fast-channel? any))))


(struct fast-channel
  ([head #:mutable]
   [tail #:mutable]
   lock bell evt)
  #:property prop:evt (struct-field-index evt))


(define (make-fast-channel)
  (recursive (channel)
    (fast-channel #f
                  #f
                  (make-semaphore 1)
                  (make-semaphore 0)
                  (guard-evt (λ _ (make-evt channel))))))

(define (make-evt channel)
  (wrap-evt (fast-channel-bell channel)
            (λ _ (call-with-semaphore (fast-channel-lock channel)
                                      (λ _ (apply values (get channel)))))))

(define (fast-channel-get channel)
  (sync channel))

(define (fast-channel-try-get channel)
  (sync/timeout 0 channel))

(define (fast-channel-put channel . args)
  (call-with-semaphore (fast-channel-lock channel)
                       (λ _ (put channel args))))

(define (put channel value)
  (cond
    ((fast-channel-tail channel)
     (let ([new-tail (mcons value #f)]
           [old-tail (fast-channel-tail channel)])
       (set-mcdr! old-tail new-tail)
       (set-fast-channel-tail! channel new-tail)))

    (else
     (let ([new-link (mcons value #f)])
       (set-fast-channel-head! channel new-link)
       (set-fast-channel-tail! channel new-link))))

  (semaphore-post (fast-channel-bell channel)))

(define (get channel)
  (let ([old-head (fast-channel-head channel)])
    (producing ((value (mcar old-head)))
      (set-fast-channel-head! channel (mcdr old-head))
      (unless (mcdr old-head)
        (set-fast-channel-tail! channel #f)))))


; vim:set ts=2 sw=2 et:
