#lang racket/base
;
; Better Exceptions
;

(require unstable/error)

(provide throw)


;; Raise an exception of defined type.
(define-syntax throw
  (syntax-rules ()
    ((_ (make-exn exn-arg ...) compose-arg ...)
     (raise (make-exn (compose-error-message compose-arg ...)
                      (current-continuation-marks)
                      exn-arg ...)))

    ((_ make-exn compose-arg ...)
     (raise (make-exn (compose-error-message compose-arg ...)
                      (current-continuation-marks))))))


; vim:set ts=2 sw=2 et:
