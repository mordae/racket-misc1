#lang scribble/manual

@require[(for-label racket)
         (for-label "reject.rkt")]

@title{Rejects}

@defmodule[misc1/reject]

When exceptions are not welcome and failure is part of the usual process,
there needs to be a way to tag unusual results. These rejects should be
accompanied with a text that explains their specific character and be
context-specific.

@defstruct*[reject ((info string?) (data any/c)) #:transparent]{
  Structure representing a generic rejected value.
  Extend as needed.
}


@; vim:set ft=scribble sw=2 ts=2 et:
