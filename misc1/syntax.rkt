#lang racket/base
;
; Syntax Extensions
;

(require racket/port)

(provide
  (all-defined-out))


;; Bind value to a name and perform a few operations,
;; producing the original value.
(define-syntax producing
  (syntax-rules ()
    ((_ ((name value) ...) body ...)
     (letrec ((name value) ...)
       (begin body ...)
       (values name ...)))

    ((_ (name value) body ...)
     (letrec ((name value))
       (begin body ...)
       name))

    ((_ name value ...)
     (letrec ((name (begin value ...)))
       name))))


;; Bind value to name and perform a few operations.
;; Returns `#<void>`.
(define-syntax using
  (syntax-rules ()
    ((_ ((name value) ...) body ...)
     (letrec ((name value) ...)
       (begin body ...)
       (void)))

    ((_ (name value) body ...)
     (letrec ((name value))
       (begin body ...)
       (void)))))


;; Bind value to name and perform a few operations,
;; provided the value is true.  Returns `#<void>`.
(define-syntax when*
  (syntax-rules ()
    ((_ ((name value) ...) body ...)
     (letrec ((name value) ...)
       (when (and name ...)
         (begin body ...)
         (void))))

    ((_ (name value) body ...)
     (letrec ((name value))
       (when name
         (begin body ...)
         (void))))))


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


;; Alias of lambda accepting any number of arguments, ignoring them all.
(define-syntax-rule (λ_ body ...)
  (λ _ body ...))


;; Alias of thunk - a lambda not accepting any arguments.
(define-syntax-rule (Λ body ...)
  (λ () body ...))


;; Shortcut of call-with-semaphore.
(define-syntax-rule (with-semaphore sema body ...)
  (call-with-semaphore sema (λ _ body ...)))


;; Shortcuts of byte and string streams.
(define-syntax-rule (with-output-bytes body ...)
  (with-output-to-bytes (λ () body ...)))

(define-syntax-rule (with-input-bytes bstr body ...)
  (with-input-from-bytes bstr (λ () body ...)))

(define-syntax-rule (with-output-string body ...)
  (with-output-to-string (λ () body ...)))

(define-syntax-rule (with-input-string str body ...)
  (with-input-from-string str (λ () body ...)))


; vim:set ts=2 sw=2 et:
