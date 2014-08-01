#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label "syntax.rkt")]

@define[syntax-eval (make-base-eval)]
@interaction-eval[#:eval syntax-eval (require "syntax.rkt")]

@title{Syntax Extensions}

@defmodule[misc1/syntax]


@defform*[((producing (name value) body ...)
           (producing name value ...))]{
  Bind @racket[value] to @racket[name] and perform a few operations,
  producing the original @racket[value] or, in the alternative form,
  just produce the value without any additional body.

  @examples[#:eval syntax-eval
    (producing (it (+ 1 2 3))
      (printf "it = ~s\n" it))
    (producing hello
      (λ () (hello)))
  ]
}

@defform*[((using ((name value) ...) body ...)
           (using (name value) body ...))]{
  Bind values to names and perform a few operations,
  producing @racket[(void)].

  @examples[#:eval syntax-eval
    (using ((it 40)
            (cf -1))
      (printf "result = ~s\n" (add1 (* cf it))))
  ]
}

@defform*[((when* ((name value) ...) body ...)
           (when* (name value) body ...))]{
  Bind values to names and perform a few operations, provided the
  values are @racket[#true].  Returns @racket[(void)].

  @examples[#:eval syntax-eval
    (when* (it (* 42/13 26/2))
      (printf "result = ~s\n" it))
    (when* ((foo 1) (bar 2))
      (printf "foo = ~s, bar = ~s\n" foo bar))
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

@defform[(with-semaphore sema body ...)]{
  Shortcut of @racket[(call-with-semaphore sema (λ _ body ...))].

  @examples[#:eval syntax-eval
    (let ((sema (make-semaphore 1)))
      (with-semaphore sema
        'protected))
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:

