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

(defparameter *var* nil)
(defparameter *threads-saved* nil)

(dolist (old-thread (all-threads))
        (destroy-thread old-thread))

(is (length (all-threads)) 0 "Empty thread list.")

(defparameter *thread*
  (make-thread (lambda ()
                 (setf *threads-saved* (all-threads))
                 (is (length *threads-saved*) 1 "One active thread.")
                 (when *threads-saved*
                   (is (thread-alive-p (car *threads-saved*)) T
                       "Thread is alive."))
                 (setf *var* :set))))

(is (length (all-threads)) 0 "No active threads.")

(when *threads-saved*
  (is (thread-alive-p (car *threads-saved*)) nil
      "Thread is no longer alive."))

(is *var* :set "The thread executed.")

;; Test Futures
(defparameter *future-one* (make-green-future))

(defparameter *val1* nil)
(defparameter *val2* nil)

;; WAIT-ON test
(with-green-thread
  (multiple-value-bind (v1 v2) (wait-on *future-one*)
    (setf *val1* v1)
    (setf *val2* v2)))

(is *val1* nil "The first value has not yet been set.")
(is *val2* nil "The second value has not yet been set.")
(is (future-complete-p *future-one*) nil
    "The future is not yet complete.")

(complete-future *future-one* :foo :bar)

(is *val1* :foo "The first value has been set.")
(is *val2* :bar "The second value has been set.")
(is (future-complete-p *future-one*) T
    "The future is now complete.")

;; WAIT-ON already completed future
(with-green-thread
  (multiple-value-bind (v1 v2) (wait-on *future-one*)
    (setf *val1* v2)
    (setf *val2* v1)))

(is *val1* :bar "The first value has been set again.")
(is *val2* :foo "The second value has been st again.")

;; Test THREAD-JOIN

(defparameter *thread1* nil)
(defparameter *future* (make-green-future))

(setf *thread1*
      (with-green-thread
        (wait-on *future*)))

(is (threadp *thread1*) T "WITH-GREEN-THREAD returned a thread object.")

(defparameter *thread-joined* nil)

(with-green-thread
  (join-thread *thread1*)
  (setf *thread-joined* T))

(is *thread-joined* nil "The thread has not yet joined.")

(complete-future *future*)

(is *thread-joined* T "The thread has now joined.")

;; CHANNEL
(defparameter *x* 0)
(let ((chan (make-instance 'channel)))
  (with-green-thread
    (with-green-thread
      (setf *x* (recv/cc chan)))
    (with-green-thread
      (send/cc chan 1))))
(is *x* 1 "1 received.")

(setf *x* 0)
(let ((chan (make-instance 'channel)))
  (with-green-thread
    (with-green-thread
      (send/cc chan 1))
    (with-green-thread
      (setf *x* (recv/cc chan)))))
(is *x* 1 "1 received.")

(let ((chan (make-instance 'channel))
      (z 0))
  (with-green-thread
    (dotimes (x 10)
      (with-green-thread
        (send/cc chan x)))
    (dotimes (y 10)
      (with-green-thread
        (when (= (recv/cc chan) y) 
          (incf z)))))
  (is z 10 "Messages received."))

;; non-blocking 
(let ((chan (make-instance 'channel)))
  (with-green-thread
    (is (send/cc chan "message" :blockp nil) nil "non-blocking SEND test.")))

(let ((chan (make-instance 'channel)))
  (with-green-thread
    (is (multiple-value-list (recv/cc chan :blockp nil)) (list nil nil) 
        "non-blocking RECV test.")))

;; this test is sensitive to deterministic nature of green-threads
(let ((chan (make-instance 'channel))
      (z 0))
  (with-green-thread
    (dotimes (x 10)
      (with-green-thread
        (send/cc chan x)))
    (dotimes (y 10)
      (with-green-thread
        (when (= (recv/cc chan :blockp nil) y) 
          (incf z)))))
  (is z 10 "Messages received."))

(finalize)
