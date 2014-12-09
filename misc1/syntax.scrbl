#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label misc1/syntax)]

@define[syntax-eval (make-base-eval)]
@interaction-eval[#:eval syntax-eval (require misc1/syntax)]

@title{Syntax Extensions}

@defmodule[misc1/syntax]


@defform[(producing ((name value) ...) body ...)]{
  Recursively binds values to names for the duration of the body,
  producing the original values.

  @examples[#:eval syntax-eval
    (producing ((a (+ 1 2 3))
                (b (* 1 2 3)))
      (printf "a = ~s, b = ~s\n" a b))
  ]
}

@defform[(using ((name value) ...) body ...)]{
  Recursively binds values to names for the duration of the body,
  producing @racket[(void)].

  @examples[#:eval syntax-eval
    (using ((variable 40)
            (coefficient -13))
      (printf "result = ~s\n" (add1 (* variable coefficient))))
  ]
}

@defform[(when* ((name value) ...) body ...)]{
  Bind values to names and perform a few operations, provided the
  values are @racket[#true].  Returns @racket[(void)].

  @examples[#:eval syntax-eval
    (when* ((x 1)
            (y 2))
      (printf "x = ~s, y = ~s\n" x y))
  ]
}

@defform[(recursive (name ...) body ...)]{
  Bind values produced by the body to specified names in a recursive manner.
  This form can be used to produce self-referential events and similar
  exotic constructs.

  @examples[#:eval syntax-eval
    (recursive (a b)
      (values 1 2))
    ((recursive (fact)
       (λ (n)
         (if (= 0 n) 1
             (* n (fact (sub1 n)))))) 5)
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

@defform[(with-semaphore sema body ...)]{
  Shortcut of @racket[(call-with-semaphore sema (λ _ body ...))].

  @examples[#:eval syntax-eval
    (let ((sema (make-semaphore 1)))
      (with-semaphore sema
        'protected))
  ]
}

@defform[(spawn-thread body ...)]{
  Shortcut of @racket[(thread (λ () body ...))].

  @examples[#:eval syntax-eval
    (thread-wait
      (spawn-thread 'do-something))
  ]
}

@defform[(with-output-bytes body ...)]{
  Shortcut of @racket[(with-output-to-bytes (λ _ body ...))].

  @examples[#:eval syntax-eval
    (with-output-bytes
      (write-byte 64))
  ]
}

@defform[(with-input-bytes bstr body ...)]{
  Shortcut of @racket[(with-input-from-bytes bstr (λ _ body ...))].

  @examples[#:eval syntax-eval
    (with-input-bytes #"hello"
      (read-byte))
  ]
}

@defform[(with-output-string body ...)]{
  Shortcut of @racket[(with-output-to-string (λ _ body ...))].

  @examples[#:eval syntax-eval
    (with-output-string
      (write-byte 64))
  ]
}

@defform[(with-input-string str body ...)]{
  Shortcut of @racket[(with-input-from-string str (λ _ body ...))].

  @examples[#:eval syntax-eval
    (with-input-string "hello"
      (read-byte))
  ]
}


@defform[(when-defined name body ...)]{
  Expands the body only when given @racket[name] is defined.

  @examples[#:eval syntax-eval
    (when-defined replace-evt 'have-replace-evt)
    (when-defined frobnicate 'have-frobnication)
    (when-defined foldl 'have-folds)
  ]
}


@defform[(~> input filters ...)]{
  Thread @racket[input] values through series of @racket[filters].

  @examples[#:eval syntax-eval
    (~> (values 1 2 3) + -)
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
