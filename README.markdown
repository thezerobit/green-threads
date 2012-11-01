# Green-Threads

A lightweight thread / cooperative multitasking library for Common Lisp.

## Usage

Allows for cooperative multitasking with help of CL-CONT for
continuations. Tries to mimic BORDEAUX-THREADS API as much as possible.

```common-lisp
(use :green-threads)
;; verbose version using CL-CONT directly
(make-thread
  (cl-cont:with-call/cc
    (lambda ()
      (do-something)
      (cl-cont:without-call/cc
        (make-thread
          (cl-cont:with-call/cc
            (lambda ()
              (do-something-else)))))
      (thread-yield) ;; allow other threads to run
      (do-more))))

;; Same as above, but with-green-thread macro creates new thread
;; and sets up with-call/cc and wraps body in (lambda () ... )
(with-green-thread
  (do-something)
  (with-green-thread
    (do-something-else))
  (thread-yield) ;; allow other threads to run
  (do-more))
```

## Installation

Clone repo into ~/quicklisp/local-projects. Run the following command:

```common-lisp
(ql:quickload :green-threads)
```

## TODO

Futures.

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License.

