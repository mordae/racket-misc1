#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label misc1/match)]

@define[match-eval (make-base-eval)]
@interaction-eval[#:eval match-eval (require racket/match misc1/match)]

@title{Match Expanders}

@defmodule[misc1/match]


@defform[(hash-lookup (key value) ...)]{
  Simple hash table descructuring expander compatible with Typed Racket.

  @examples[#:eval match-eval
    (match #hasheq((temperature . 34)
                   (day-time . "afternoon"))
      ((hash-lookup ('temperature te)
                    ('day-time dt))
       (format "beautiful ~a, ~a°C" dt te)))
  ]
}

@; vim:set ft=scribble sw=2 ts=2 et:
