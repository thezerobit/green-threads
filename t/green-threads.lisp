#|
  This file is a part of green-threads project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage green-threads-test
  (:use :cl
        :green-threads
        :cl-test-more))
(in-package :green-threads-test)

(plan nil)

;; Test Futures
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

;; WAIT-FOR already completed future
(with-green-thread
  (multiple-value-bind (v1 v2) (wait-for *future-one*)
    (setf *val1* v2)
    (setf *val2* v1)))

(is *val1* :bar)
(is *val2* :foo)

;; Test THREAD-JOIN

(defparameter *thread1* nil)
(defparameter *future* (make-future))

(setf *thread1*
      (with-green-thread
        (wait-for *future*)))

(is (threadp *thread1*) T)

(defparameter *thread-joined* nil)

(with-green-thread
  (join-thread *thread1*)
  (setf *thread-joined* T))

(is *thread-joined* nil)

(complete-future *future*)

(is *thread-joined* T)

(finalize)
