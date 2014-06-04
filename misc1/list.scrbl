#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket racket/dict)
         (for-label "list.rkt")]

@define[list-eval (make-base-eval)]
@interaction-eval[#:eval list-eval (require "list.rkt")]

@title{Lists}

@defmodule[misc1/list]

@defproc[(list->values (lst list?)) any]{
  Convert a list to multiple return values.
  Basically just a shortcut for @racket[(apply values lst)].

  @examples[#:eval list-eval
    (list->values '(1 2 3))
  ]
}

@defproc[(values* (value any/c) ... (list-of-values list?)) any]{
  Analogous to @racket[list*] but instead of creating a list,
  produces multiple return values.

  @examples[#:eval list-eval
    (values* 1 '(2 3))
  ]
}

@defproc[(split-every (lst list?) (idx exact-positive-integer?))
         (listof list?)]{
  Repeatedly split specified list at the given offset and produce
  multiple lists with given number of items.

  @examples[#:eval list-eval
    (split-every '(1 2 3 4 5 6) 2)

    (for/list ((part (split-every '(1 2 3 4 5 6) 3)))
      (apply + part))
  ]
}

@defform[(let-list (((name ...) value) ...) body ...)]{
  Similar to @racket[let-values], but consuming lists instead of
  multiple value returns.

  @examples[#:eval list-eval
    (let-list (((a b c) '(1 2 3))
               ((d e f) '(4 5 6)))
      (+ a b c d e f))
  ]
}

@; vim:set ft=scribble sw=2 ts=2 et:
