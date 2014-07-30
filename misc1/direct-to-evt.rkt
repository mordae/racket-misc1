#lang racket/base
;
; Making Blocking Code Asynchronous
;

(require racket/contract
         racket/control
         racket/port)

(provide
  (contract-out
    (direct->evt/input (-> input-port? (-> input-port? any) evt?))
    (direct->evt (-> (-> (-> evt? any) any) evt?))))


(define (abort-to-scheduler tag evt)
  (define (restart resume)
    (define (restore/cc . args)
      (apply call/prompt resume tag values args))
    (abort/cc tag (replace-evt evt restore/cc)))
  (call/cc restart tag))

(define (aborting-input-port in tag)
  (define-values (buffer-out buffer-in)
    (make-pipe 4096))

  (define temp
    (make-bytes 4096))

  (define (aborting-read! bstr)
    (let ((received (read-bytes-avail!* temp in)))
      (cond
        ((equal? received 0)
         (abort-to-scheduler tag (wrap-evt in (λ _ 0))))

        ((eof-object? received)
         (values eof))

        (else
         (write-bytes temp buffer-in 0 received)
         (values buffer-out)))))

  (make-input-port/read-to-peek 'aborting-input-port
                                aborting-read!
                                #f void))

(define (direct->evt/input in proc)
  (let* ((tag (make-continuation-prompt-tag 'block->evt/input))
         (in/a (aborting-input-port in tag)))
    (define (start)
      (let ((result (proc in/a)))
        (wrap-evt always-evt (λ _ result))))

    (replace-evt always-evt
                 (λ _ (call/prompt start tag values)))))

(define (direct->evt proc)
  (let ((tag (make-continuation-prompt-tag 'block->evt)))
    (define (wait evt)
      (abort-to-scheduler tag evt))

    (define (start)
      (let ((result (proc wait)))
        (wrap-evt always-evt (λ _ result))))

    (replace-evt always-evt
                 (λ _ (call/prompt start tag values)))))


; vim:set ts=2 sw=2 et:
