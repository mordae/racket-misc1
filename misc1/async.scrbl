#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label "async.rkt")]

@define[async-eval (make-base-eval)]
@interaction-eval[#:eval async-eval (require "async.rkt")]

@title{Asynchronous Tasks}

@defmodule[misc1/async]

@defproc[(async-proc (proc (-> any))) evt?]{
  Run thunk in a @racket[thread] and obtain it's result (or exception)
  using a synchronous channel.
}

@defform[(async body ...)]{
  Syntactic alternative to @racket[async-proc].
}


@; vim:set ft=scribble sw=2 ts=2 et:
