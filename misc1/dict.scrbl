#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket racket/dict racket/undefined)
         (for-label "dict.rkt")]

@define[dict-eval (make-base-eval)]
@interaction-eval[#:eval dict-eval (require "dict.rkt")]

@title{Dictionaries}

@defmodule[misc1/dict]

@defproc[(dict-mref (dict dict?)
                    (#:default default any/c undefined)
                    (key any/c) ...)
         any]{
  Return values of multiple dictionary keys at once.

  The optional @racket[default] keyword argument allows to define a
  substitute for mising keys' values.  If a procedure is specified,
  it's return value is used.

  @examples[#:eval dict-eval
    (dict-mref (hasheq 'apples 4 'pears 2) 'apples 'pears)
    (dict-mref (hasheq 'pomelos 1) 'peaches #:default 0)
  ]
}

@defproc[(dict-merge (base dict?) (other dict?) ...) dict?]{
  Merge multiple dictionaries.
  Type of the base dictionary dictes type of the result.

  @examples[#:eval dict-eval
    (dict-merge (hasheq 'apples 1) '((pears . 2)))
  ]
}

@defproc[(dict-merge! (base dict?) (other dict?) ...) void?]{
  Merge multiple dictionaries into the base one.
  The base dictionary is mutated multiple times.
}

@defform[(let-dict (((name ...) value) ...) body ...)]{
  Similar to let-values, but instead of multiple return values
  consuming dictionaries with keys identical to target names.

  @examples[#:eval dict-eval
    (let-dict (((a b c) (hasheq 'a 1 'b 2 'c 3))
               ((d e f) '((d . 4) (e . 5) (f . 6))))
      (+ a b c d e f))
  ]
}

@; vim:set ft=scribble sw=2 ts=2 et:
