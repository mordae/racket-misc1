#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label "fast-channel.rkt")]

@define[fast-channel-eval (make-base-eval)]
@interaction-eval[#:eval fast-channel-eval (require "fast-channel.rkt")]

@title{Fast Channels}

@defmodule[misc1/fast-channel]

Fast channels are an alternative to @racket[racket/async-channel]
that makes use of semaphores instead of a background thread,
yielding a much greater throughput.

@defproc[(make-fast-channel) fast-channel?]{
  Create new, empty fast channel.

  @examples[#:eval fast-channel-eval
    (define channel (make-fast-channel))
  ]
}

@defproc[(fast-channel? (v any/c)) boolean?]{
  Identifies a fast channel instance.

  @examples[#:eval fast-channel-eval
    (fast-channel? channel)
  ]
}

@defproc[(fast-channel-put (channel fast-channel?)
                           (value any/c) ...)
         void?]{
  Send another item through the channel.

  There is no limit on number of items placed into a channel and
  the caller is never blocked.

  @examples[#:eval fast-channel-eval
    (fast-channel-put channel 'carrot)
    (fast-channel-put channel 'apple)
    (fast-channel-put channel 'rose 'and 'cherry)
  ]
}

@defproc[(fast-channel-get (channel fast-channel?)) any]{
  Remove an item from the channel in FIFO mode.
  The caller is blocked when the channel is empty.
  Since the channel works as a synchronizable event,
  it is possible to wait for items to arrive asynchronously.

  @examples[#:eval fast-channel-eval
    (fast-channel-get channel)
    (sync channel)
  ]
}

@defproc[(fast-channel-try-get (channel fast-channel?)) any]{
  Try to wait for an item from the channel but return just
  @racket[#f] if the waiting would block.

  Please note that it is possible to send multiple values,
  but this function fails with just one. Make sure you expect
  proper return arity.

  @examples[#:eval fast-channel-eval
    (fast-channel-try-get channel)
    (fast-channel-try-get channel)
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
