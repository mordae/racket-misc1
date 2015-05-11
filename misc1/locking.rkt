#lang racket/base
;
; Single-Writer / Multiple Readers Locking Primitive
;

(require racket/contract
         racket/function)

(require misc1/syntax)

(provide with-read-lock
         with-write-lock)

(provide
  (contract-out
    (rwlock? predicate/c)
    (make-rwlock (->* () (semaphore?) rwlock?))
    (call-with-read-lock (-> rwlock? (-> any) any))
    (call-with-write-lock (-> rwlock? (-> any) any))))


(struct rwlock
  (wlock rlock (readers #:mutable)))


(define (make-rwlock (wlock (make-semaphore 1)))
  (let ((rlock (make-semaphore 1)))
    (rwlock wlock rlock 0)))


(define-syntax-rule (with-read-lock rw-l body ...)
  (call-with-read-lock rw-l (thunk body ...)))

(define-syntax-rule (with-write-lock rw-l body ...)
  (call-with-write-lock rw-l (thunk body ...)))


(define (call-with-read-lock rw-l proc)
  (after
    (begin
      (rwlock-read-lock rw-l)
      (proc))
    (cleanup
      (rwlock-read-unlock rw-l))))

(define (call-with-write-lock rw-l proc)
  (after
    (begin
      (rwlock-write-lock rw-l)
      (proc))
    (cleanup
      (rwlock-write-unlock rw-l))))


(define (rwlock-read-lock rw-l)
  (with-semaphore (rwlock-rlock rw-l)
    (let ((readers (add1 (rwlock-readers rw-l))))
      (when (= 1 readers)
        (semaphore-wait/enable-break (rwlock-wlock rw-l)))
      (set-rwlock-readers! rw-l readers))))

(define (rwlock-read-unlock rw-l)
  (with-semaphore (rwlock-rlock rw-l)
    (let ((readers (sub1 (rwlock-readers rw-l))))
      (when (= 0 readers)
        (semaphore-post (rwlock-wlock rw-l)))
      (set-rwlock-readers! rw-l readers))))


(define (rwlock-write-lock rw-l)
  (semaphore-wait/enable-break (rwlock-wlock rw-l)))

(define (rwlock-write-unlock rw-l)
  (semaphore-post (rwlock-wlock rw-l)))


; vim:set ts=2 sw=2 et:
