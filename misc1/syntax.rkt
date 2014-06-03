#lang racket/base
;
; Syntax Extensions
;

(provide producing using when* loop λ_)


;; Bind value to a name and perform a few operations,
;; producing the original value.
(define-syntax-rule (producing (name value) body ...)
  (let ((name value))
    (begin body ...)
    name))


;; Bind value to name and perform a few operations.
;; Returns `#<void>`.
(define-syntax-rule (using (name value) body ...)
  (let ((name value))
    (begin body ...)
    (void)))


;; Bind value to name and perform a few operations,
;; provided the value is true.  Returns `#<void>`.
(define-syntax-rule (when* (name value) body ...)
  (let ((name value))
    (when name
      (begin body ...))))


;; Loop body indefinitely.
(define-syntax-rule (loop body ...)
  (let loop ()
    (begin body ...)
    (loop)))


;; Alias of lambda accepting any number of arguments, ignoring them all.
(define-syntax-rule (λ_ body ...)
  (λ _ body ...))


; vim:set ts=2 sw=2 et:
