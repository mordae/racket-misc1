#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label "syntax.rkt")]

@define[syntax-eval (make-base-eval)]
@interaction-eval[#:eval syntax-eval (require "syntax.rkt")]

@title{Syntax Extensions}

@defmodule[misc1/syntax]


@defform[(producing (name value) body ...)]{
  Bind @racket[value] to @racket[name] and perform a few operations,
  producing the original @racket[value].

  @examples[#:eval syntax-eval
    (producing (it (+ 1 2 3))
      (printf "it = ~s\n" it))
  ]
}

@defform[(using (name value) body ...)]{
  Bind value to name and perform a few operations, producing @racket[(void)].

  @examples[#:eval syntax-eval
    (using (it 40)
      (+ it 2))
  ]
}

@defform[(when* (name value) body ...)]{
  Bind value to name and perform a few operations, provided the
  value is @racket[#true].  Returns @racket[(void)].

  @examples[#:eval syntax-eval
    (when* (it (* 42/13 26/2))
      (printf "result = ~s\n" it))
  ]
}

@defform[(loop body ...)]{
  Loop body indefinitely.
}

@defform[(while continue? body ...)]{
  Loop body as long as the condition holds.
}

@defform[(until halt? body ...)]{
  Loop body as long as the condition does not hold.
}

@defform[(loop-while-cond (test body ...) ...)]{
  Loop as long as any cond-style clause matches.
  The else clause is reserved.

  @examples[#:eval syntax-eval
    (loop-while-cond
      ((< (random) 0.2)  (display "jackpot!\n"))
      ((< (random) 0.7)  (display "hit!\n")))
  ]
}

@defform[(loop-until-cond (test body ...) ...)]{
  Loop until any cond-style clause matches.
  The else clause is reserved.

  @examples[#:eval syntax-eval
    (loop-until-cond
      ((> (random) 0.5)  (display "pressure too high!"))
      ((> (random) 0.3)  (display "boiler cracking!"))
      ((> (random) 0.1)  (display "office is a pressure coo...!")))
  ]
}

@defform[(λ_ body ...)]{
  Alias of @racket[λ] accepting any number of arguments, ignoring them all.

  @examples[#:eval syntax-eval
    (map (λ_ 1) '(a b c))
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:

