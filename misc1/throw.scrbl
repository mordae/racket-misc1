#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket
                    unstable/error)
         (for-label misc1/throw)]

@define[throw-eval (make-base-eval)]
@interaction-eval[#:eval throw-eval (require misc1/throw)]

@title{Exceptions}

@defmodule[misc1/throw]

@defform*[((throw make-exn compose-arg ...)
           (throw (make-exn exn-arg ...) compose-arg ...))]{
  Compose error message using the
  @racket[(compose-error-message compose-arg ...)] formula and then
  pass it along with additional exception arguments to the defined
  exception constructor as @racket[(make-exn message marks exn-arg ...)].

  Raise the resulting exception.

  @examples[#:eval throw-eval
    (struct exn:fail:example exn:fail (number))
    (throw (exn:fail:example 42)
           'example "failed to understand"
                    "subject" "universe and everything")
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
