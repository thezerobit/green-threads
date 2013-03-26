# Green-Threads

A lightweight thread / cooperative multitasking library for Common Lisp.

## Usage

Allows for cooperative multitasking with help of CL-CONT for
continuations. Tries to mimic BORDEAUX-THREADS API as much as possible.

Let's show how the threads work and build up to higher level
abstractions.

The MAKE-THREAD function takes a closure/function of zero arguments and
starts executing it, since we are not using OS threads, it cannot
actually preempt the thread, so the thread has to yield by calling
QUEUE-NEXT, passing a closure containing the continuation of the thread
and return. MAKE-THREAD has an optional keyword parameter :NAME for
specifying a name for the thread.

In this example, we create a thread which executes immediately, it
creates an additional thread and then continues on, it yields by calling
QUEUE-NEXT and passing a continuation. That allows the other thread to
execute before continuing the first:

```common-lisp
(use-package :green-threads)

(make-thread
  (lambda ()
    (format t "Do Something~%")
    (make-thread
      (lambda ()
        (format t "Do Something Else.~%")))
    (queue-next ;; queue next action and yield
      (lambda ()
        (format t "Do More.~%")))))

;; output:
;; Do Something
;; Do Something Else.
;; Do More.
```

In the above example, the continuation of the primary thread is nested
inside a QUEUE-NEXT call. That's pretty inconvenient. Nobody enjoys
programming in continuation-passing-style (CPS), so let's see how we can
avoid that.

In the next example, we use the CL-CONT library directly to transform
our code into CPS which allows us to use THREAD-YIELD to yield without
having to specify the continuation ourselves. You'll notice that the
primary thread no longer has to nest the continuation in a closure.

```common-lisp
(make-thread
  (cl-cont:with-call/cc
    (lambda ()
      (format t "Do Something~%")
      (cl-cont:without-call/cc
        (make-thread
          (cl-cont:with-call/cc
            (lambda ()
              (format t "Do Something Else~%")))))
      (thread-yield) ;; allow other threads to run
      (format t "Do More~%"))))
```

The last example is a bit noisy with the CL-CONT calls, so we can
instead use the WITH-GREEN-THREAD macro which wraps our code in an
anonymous closure, and applies CL-CONT macros appropriately.

```common-lisp
(with-green-thread
  (format t "Do Something~%")
  (with-green-thread
    (format t "Do Something Else~%"))
  (thread-yield) ;; allow other threads to run
  (format t "Do More~%"))
```

Viola, you can now write cooperatively multitasking code without
resorting to writing CPS by hand.

Dynamic bindings don't over well into green threads, so this library has
a similar mechanism as BORDEAUX-THREADS, and that is the
\*DEFAULT-SPECIAL-BINDINGS\* special variable which can be set to an
alist of symbol/value pairs that will be specially bound in any green
threads you create.

```common-lisp
(defparameter *foo* "outer")

(with-green-thread
  (print *foo*)
  (let ((*default-special-bindings* '((*foo* . "first"))))
    (with-green-thread
      (print *foo*)))
  (thread-yield)
  (print *foo*))

;; output:
;; "outer"
;; "first"
;; "outer"
```

MAKE-THREAD creates a new green thread and runs it if no green threads
are currently running. If called from within a running green thread, it
will queue the thread to run later.

CURRENT-THREAD returns the currently running thread object or NIL if you
are not currently in one.

THREADP returns T if the object passed to it is a thread object.

THREAD-NAME returns the name of the thread passed to it or NIL of none
was provided.

ALL-THREADS returns a list of all threads.

DESTROY-THREAD will do just that, cannot be called on currently
executing thread.

THREAD-ALIVE-P returns T if thread passed to it is still alive.

JOIN-THREAD (thread) Requires CL-CONT:WITH-CALL/CC environment, causes
the current thread to wait for the other thread to complete before
continuing.

### Futures

MAKE-FUTURE () creates a future object.

QUEUE-FUTURE (future action &optional thread) queues an action on
current (or specified) thread to take place when provided future is
completed.

FINISH (future &rest values) signals that a future is complete
and provides return values for the future.

FUTURE-FINISHED-P (future) T if future has already had COMPLETE-FUTURE
called on it.

FUTURE-VALUES (future) Returns the values given to the future when it
was completed.

WAIT-ON (future) Requires CL-CONT:WITH-CALL/CC environment, causes the
current thread to wait for the completion of the specified future and
returns the values given to the future when it was completed.

GET-JOIN-FUTURE (thread) Returns a future which will complete when the
passed in thread completes. This provides a lower level way to join
threads without using JOIN-THREAD which requires CL-CONT:WITH-CALL/CC.

Example use of futures from (taken from tests):

```common-lisp
(defparameter *future-one* (make-future))

(defparameter *val1* nil)
(defparameter *val2* nil)

;; WAIT-FOR test
(with-green-thread
  (multiple-value-bind (v1 v2) (wait-for *future-one*)
    (setf *val1* v1)
    (setf *val2* v2)))

(is *val1* nil)
(is *val2* nil)
(is (future-complete-p *future-one*) nil)

(complete-future *future-one* :foo :bar)

(is *val1* :foo)
(is *val2* :bar)
(is (future-complete-p *future-one*) T)
```

### Channels

Inspired by sykopomp's [ChanL](https://github.com/sykopomp/chanl), which
relies on BORDEAUX-THREADS, I've added basic unbuffered channel support.

(make-instance 'CHANNEL) creates a new channel.

SEND/CC (channel value &key blockp) Requires CL-CONT:WITH-CALL/CC
environment. Sends a value to a channel, and blocks, unless blockp is
nil. Returns the channel that received the value unless blockp is nil
and there is no thread waiting to receive in which case it returns nil.

RECV/CC (channel &key blockp) Requires CL-CONT:WITH-CALL/CC environment.
Receives a value from a channel, and blocks, unless blockp is nil.
Returns 2 values, the first is the value being received and the second
is a generalized boolean that is only nil if blockp is nil and there is
no thread waiting to send a value.

SEND (channel value continuation &key blockp) Just like SEND/CC but
doesn't require CL-CONT, you just have to pass in the continuation
manually. CPS is fun. Instead of returning the channel that receives the
message, it (or nil) is passed to continuation.

RECV (channel continuation &key blockp) Just like RECV/CC but doesn't
require CL-CONT, you just have to pass in the continuation manually.
CPS is fun. Instead of returning the 2 values, the continuation is
called with them.

Behold, the venerable sieve of Eratosthenes:

```common-lisp
(ql:quickload :green-threads)

(cl-cont:defun/cc counter (end chan)
  (loop for i from 2 to end
        do (gt:send/cc chan i)))

(declaim (ftype function filter)) ;; unnecessary, silence warnings

(cl-cont:defun/cc filter (listen)
  (let ((prime (gt:recv/cc listen)))
    (format t "~a " prime)
    (let ((chan (make-instance 'gt:channel)))
      (gt:with-green-thread
        (filter chan))
      (loop
        (let ((i (gt:recv/cc listen)))
          (if (> (mod i prime) 0)
            (gt:send/cc chan i)))))))

(gt:with-green-thread
  (let ((count (make-instance 'gt:channel)))
    (gt:with-green-thread
      (counter 100 count))
    (gt:with-green-thread
      (filter count))))
```

## Installation

Clone repo into ~/quicklisp/local-projects. Run the following command:

```common-lisp
(ql:quickload :green-threads)
```

## TODO

100% test coverage.

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License.

