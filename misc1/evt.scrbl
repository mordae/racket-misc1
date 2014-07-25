#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label "evt.rkt")]

@define[evt-eval (make-base-eval)]
@interaction-eval[#:eval evt-eval (require racket/async-channel)]
@interaction-eval[#:eval evt-eval (require "evt.rkt")]

@title{Events}

Extended events, some building on the new @racket[replace-evt] procedure.

@defmodule[misc1/evt]


@defproc[(alarm-in-evt (msecs real?)) evt?]{
  Create an alarm event that is up in given number of milliseconds,
  counting from now. Just an useful shortcut.
}

@defproc[(timer-evt (msecs real?) (handler (-> any))) evt?]{
  Recurring event that executes the @racket[handler] after @racket[msecs]
  milliseconds. Never produces any synchronization result.

  @examples[#:eval evt-eval
    (sync (alarm-in-evt 1000)
          (timer-evt 300 (λ _ (printf "hello\n"))))
  ]
}

@defproc[(recurring-evt (base-evt evt?) (handler procedure?)) evt?]{
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


@; vim:set ft=scribble sw=2 ts=2 et:
