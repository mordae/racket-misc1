#lang scribble/manual

@require[scribble/eval]
@require["syntax.rkt"]

@require[(for-label racket)
         (for-label "rwlock.rkt")]

@define[rwlock-eval (make-base-eval)]
@interaction-eval[#:eval rwlock-eval (require "rwlock.rkt")]

@title{Read-Write Locks}

Single writer / multiple readers locking primitive.

@defmodule[misc1/rwlock]

@defproc[(make-rwlock) rwlock?]{
  Create new, unlocked read-write lock.

  @examples[#:eval rwlock-eval
    (define lock (make-rwlock))
  ]
}

@defproc[(rwlock? (v any/c)) boolean?]{
  Predicate identifying a read-write lock.

  @examples[#:eval rwlock-eval
    (rwlock? lock)
  ]
}

@defform[(with-read-lock lock body ...)]{
  Execute body protected by a shared reader lock.
  Waits for the lock to become available.

  @examples[#:eval rwlock-eval
    (with-read-lock lock
      'protected-from-writes)
  ]
}

@defform[(with-write-lock lock body ...)]{
  Execute body protected by an exclusive writer lock.
  Waits for the lock to become available.

  @examples[#:eval rwlock-eval
    (with-write-lock lock
      'exclusive-access)
  ]
}

@defproc[(call-with-read-lock (lock rwlock?) (proc (-> any))) any]{
  Executes the body @racket[proc] protected by a shared reader lock.
}

@defproc[(call-with-write-lock (lock rwlock?) (proc (-> any))) any]{
  Executes the body @racket[proc] protected by an exclusive writer lock.
}


@; vim:set ft=scribble sw=2 ts=2 et:
