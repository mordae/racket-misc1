#lang scribble/manual

@require[scribble/eval]
@require[misc1/syntax]

@require[(for-label racket)
         (for-label misc1/evt)]

@define[evt-eval (make-base-eval)]
@interaction-eval[#:eval evt-eval (require racket/async-channel)]
@interaction-eval[#:eval evt-eval (require misc1/evt)]

@title{Events}

Extended events, some building on the new @racket[replace-evt]
procedure when available.

@defmodule[misc1/evt]

@when-defined[replace-evt]{
  @defproc[(timer-evt (msecs real?) (handler (-> any))) evt?]{
    Recurring event that executes the @racket[handler] immediately and
    then repeatedly after @racket[msecs] milliseconds. It never produces
    any synchronization result.

    @examples[#:eval evt-eval
      (sync (alarm-in-evt 1000)
            (timer-evt 400 (λ _ (printf "hello\n"))))
    ]
  }

  @defproc[(recurring-evt (base-evt evt?) (handler procedure? void)) evt?]{
    Recurring event that never produces any synchronization result.

    @examples[#:eval evt-eval
      (let ((channel (make-async-channel)))
        (for ((i 3))
          (async-channel-put channel i))
        (sync (alarm-in-evt 1000)
              (recurring-evt channel
                             (λ (item)
                               (printf "item ~s\n" item)))))
    ]
  }
}


@defproc[(alarm-in-evt (msecs real?)) evt?]{
  Create an alarm event that is up in given number of milliseconds,
  counting from now. Just an useful shortcut.
}

@defproc[(constant-evt (arg any/c) ...) evt?]{
  Simple event that immediately produces specified arguments.

  @examples[#:eval evt-eval
    (sync (constant-evt 1 2 3))
  ]
}

@defproc[(cache-evt (evt evt?)) evt?]{
  Simple event that caches first result of it's parent event,
  to return it if waited for more than once.

  @examples[#:eval evt-eval
    (define parent-evt (wrap-evt always-evt
                                 (λ _ (printf "parent producing 42\n") 42)))

    (define child-evt (cache-evt parent-evt))

    (sync child-evt)
    (sync child-evt)
  ]
}

@defproc[(trigger-evt? (v any/c)) boolean?]{
  Predicate to identify trigger events.
}

@defproc[(make-trigger-evt) evt?]{
  Create an event that can be triggered later on.

  @examples[#:eval evt-eval
    (define t-e (make-trigger-evt))
    (sync/timeout 0 t-e)
  ]
}

@defproc[(trigger! (evt trigger-evt?) (v any/c) ...) void?]{
  Cause specified trigger event to stop blocking and start producing
  given results instead.

  @examples[#:eval evt-eval
    (trigger! t-e 13 42)
    (sync/timeout 0 t-e)
  ]
}

@defproc[(cancel! (evt trigger-evt?)) void?]{
  Cancel specified trigger event, causing it to block again.

  @examples[#:eval evt-eval
    (cancel! t-e)
    (sync/timeout 0 t-e)
  ]
}

@defproc[(epoch-evt? (v any/c)) boolean?]{
  Predicate to identify epoch events.
}

@defproc[(make-epoch-evt) evt?]{
  Create an event that can be triggered to unblock all waiters and start
  a new epoch, blocking newcomers.

  @examples[#:eval evt-eval
    (define ee (make-epoch-evt))
  ]
}

@defproc[(epoch-evt-advance! (evt epoch-evt?) (v any/c) ...) void?]{
  Advance given epoch event, unblocking all threads waiting for it.

  @examples[#:eval evt-eval
    (thread (λ ()
              (sleep 1)
              (epoch-evt-advance! ee 'result)))
    (sync ee)
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
