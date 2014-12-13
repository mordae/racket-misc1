#lang racket/base
;
; Syntax Extensions
;

(require racket/port)

(require
  (for-syntax racket/base))

(provide
  (all-defined-out))


;; Bind values to a names and perform a few operations,
;; producing the original values.
(define-syntax-rule (producing ((name value) ...) body ...)
  (letrec ((name value) ...)
   (begin body ...)
   (values name ...)))


;; Bind values to names and perform a few operations.
;; Returns `#<void>`.
(define-syntax-rule (using ((name value) ...) body ...)
  (letrec ((name value) ...)
   (begin body ...)
   (void)))


;; Bind values to names and perform a few operations,
;; provided the values are true.  Returns `#<void>`.
(define-syntax-rule (when* ((name value) ...) body ...)
  (letrec ((name value) ...)
    (when (and name ...)
      (begin body ...)
      (void))))


;; Produce several interdependent values.
(define-syntax-rule (recursive (name ...) body ...)
  (letrec-values (((name ...) (begin body ...)))
    (values name ...)))


;; Loop body indefinitely.
(define-syntax-rule (loop body ...)
  (let loop ()
    (begin body ...)
    (loop)))


;; Loop body as long as the condition holds.
(define-syntax-rule (while continue? body ...)
  (let loop ()
    (when continue?
      (begin body ...)
      (loop))))


;; Loop body as long as the condition does not hold.
(define-syntax-rule (until halt? body ...)
  (let loop ()
    (unless halt?
      (begin body ...)
      (loop))))


;; Loop as long as any cond-style clause matches.
;; The else clause is reserved.
(define-syntax-rule (loop-while-cond (test body ...) ...)
  (let loop ()
    (cond
      (test (begin body ...)
            (loop)) ...
      (else (void)))))


;; Loop until any cond-style clause matches.
;; The else clause is reserved.
(define-syntax-rule (loop-until-cond (test body ...) ...)
  (let loop ()
    (cond
      (test body ...) ...
      (else (loop)))))


;; Shortcut of call-with-semaphore.
(define-syntax-rule (with-semaphore sema body ...)
  (call-with-semaphore sema (λ _ body ...)))

;; Just a shortcut for (thread ...).
(define-syntax-rule (spawn-thread body ...)
  (thread (λ () body ...)))


;; Shortcuts of byte and string streams.
(define-syntax-rule (with-output-bytes body ...)
  (with-output-to-bytes (λ () body ...)))

(define-syntax-rule (with-input-bytes bstr body ...)
  (with-input-from-bytes bstr (λ () body ...)))

(define-syntax-rule (with-output-string body ...)
  (with-output-to-string (λ () body ...)))

(define-syntax-rule (with-input-string str body ...)
  (with-input-from-string str (λ () body ...)))


;; Expands body only when an identifier is defined.
(define-syntax (when-defined stx)
  (syntax-case stx ()
    ((_ name body ...)
     (if (identifier-binding #'name)
         #'(begin body ...)
         #'(void)))))


; vim:set ts=2 sw=2 et:
