#lang scribble/manual

@require[scribble/eval]
@require[misc1/syntax]

@require[(for-label racket)
         (for-label misc1/locking)]

@define[locking-eval (make-base-eval)]
@interaction-eval[#:eval locking-eval (require misc1/locking)]

@title{Advanced Locking}

Advanced locking tools, such as read-write locks and lock tables.

@defmodule[misc1/locking]

@defproc[(make-rwlock (wlock semaphore? (make-semaphore 1))) rwlock?]{
  Create new read-write lock, optionally reusing specified semaphore as
  write lock. External write lock can be useful for situations where the
  rwlock must start write-locked.

  @examples[#:eval locking-eval
    (define lock (make-rwlock))
  ]
}

@defproc[(rwlock? (v any/c)) boolean?]{
  Predicate identifying a read-write lock.

  @examples[#:eval locking-eval
    (rwlock? lock)
  ]
}

@defform[(with-read-lock lock body ...)]{
  Execute body protected by a shared reader lock.
  Waits for the lock to become available.

  @examples[#:eval locking-eval
    (with-read-lock lock
      'protected-from-writes)
  ]
}

@defform[(with-write-lock lock body ...)]{
  Execute body protected by an exclusive writer lock.
  Waits for the lock to become available.

  @examples[#:eval locking-eval
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
