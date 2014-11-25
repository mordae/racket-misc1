#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket racket/match)
         (for-label "match.rkt")]

@define[match-eval (make-base-eval)]
@interaction-eval[#:eval match-eval (require racket/match "match.rkt")]

@title{Match Extensions}

@defmodule[misc1/match]

@defform[(equal muster)]{
  New @racket[match] expander form that matches any value
  @racket[equal?] to the specified @racket[muster] expression.

  @examples[#:eval match-eval
    (let ((foo (* 2 21)))
      (match 42
        [(equal foo) 'hello]
        [else 'bye]))
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
