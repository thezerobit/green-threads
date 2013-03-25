#|
  This file is a part of green-threads project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage green-threads
  (:use :cl :cl-cont)
  (:import-from :cl-async-future
                #:finished
                #:make-future
                #:finish)
  (:export #:make-thread
           #:*default-special-bindings*
           #:current-thread
           #:threadp
           #:thread-name

           ;; locks not yet supported:
           ;; #:make-lock
           ;; #:acquire-lock
           ;; #:release-lock
           ;; #:with-lock-held
           ;; #:make-recursive-lock
           ;; #:acquire-recursive-lock
           ;; #:release-recursive-lock
           ;; #:with-recursive-lock-held

           ;; condition variables not yet supported
           ;; TODO: instead of futures?
           ;; #:make-condition-variable
           ;; #:condition-wait
           ;; #:condition-notify

           #:all-threads
           ;; #:interrupt-thread ;; N/A
           #:destroy-thread
           #:thread-alive-p
           #:join-thread

           ;; additional:
           #:queue-next
           #:thread-yield
           #:with-green-thread

           ;; futures:
           #:make-green-future
           #:queue-future
           #:finish
           #:complete-future
           #:future-complete-p
           #:wait-on
           #:get-join-future
           #:future-values

           ;; channels:
           #:channel
           #:send/cc
           #:recv/cc
           #:send
           #:recv)
  (:nicknames :gt))
(in-package :green-threads)

;; Batched-Queue
;; adapted from "Purely Function Data Structures" by Chris Okasaki
;; This is just to provide a simple FIFO queue for this library
;; internally.

(defclass batched-queue ()
  ((front :initform nil :initarg :front)
   (rear :initform nil :initarg :rear)))

(defgeneric empty-p (sequence))
(defgeneric snoc (sequence value))
(defgeneric head (sequence))
(defgeneric tail (sequence))
(defgeneric size (sequence))

;; implementation of psuedo-contructor
(defun construct-batched-queue (front rear)
  (if (null front)
    (make-instance 'batched-queue
                   :front (reverse rear))
    (make-instance 'batched-queue
                   :front front
                   :rear rear)))

(defmethod empty-p ((queue batched-queue))
  (null (slot-value queue 'front)))

(defmethod snoc ((queue batched-queue) value)
  (with-slots (front rear) queue
    (construct-batched-queue front (cons value rear))))

(defmethod head ((queue batched-queue))
  (when (empty-p queue)
    (error "Called HEAD on an empty queue."))
  (car (slot-value queue 'front)))

(defmethod tail ((queue batched-queue))
  (when (empty-p queue)
    (error "Called TAIL on an empty queue."))
  (with-slots (front rear) queue
    (construct-batched-queue (cdr front) rear)))

(defmethod size ((queue batched-queue))
  (with-slots (front rear) queue
    (+ (length front) (length rear))))

;; parameters
(defparameter *default-special-bindings* nil)
(defparameter *current-thread* nil)
(defparameter *active-threads* nil)
(defparameter *thread-queue* (make-instance 'batched-queue))

;; classes
(defclass thread ()
  ((name :initarg :name :reader name)
   (binding-symbols :initarg :binding-symbols :reader binding-symbols)
   (binding-values :initarg :binding-values :reader binding-values)
   (next-action :initform nil :accessor next-action)
   (alive :initform T :accessor alive)
   (join-future :initform (make-green-future) :reader join-future)))

;; functions
(defun bindings-from-alist (alist)
  (let ((binding-symbols nil)
        (binding-values nil))
    (dolist (cons alist)
      (destructuring-bind (symbol . value) cons
        (when (not (member symbol binding-symbols))
          (push symbol binding-symbols)
          (push value binding-values))))
    (values binding-symbols binding-values)))

(defun thread-loop ()
  (loop while (not (empty-p *thread-queue*))
        do (let ((thread (head *thread-queue*)))
             (setf *thread-queue* (tail *thread-queue*))
             (when (alive thread) ;; destroyed threads might be here
               (progv (binding-symbols thread) (binding-values thread)
                 (let ((*current-thread* thread)
                       (action (next-action thread)))
                   (when (null action)
                     (error "attempting to queue thread with no next-action"))
                   (setf (next-action thread) nil)
                   (funcall action))) 
               (if (not (next-action thread))
                 (destroy-thread thread))))))

(defun queue-next (action &optional thread)
  (let ((thread (or thread *current-thread*)))
    (when (null thread)
      (error "Must provide thread to queue-next if not already in thread."))
    (when (next-action thread)
      (error "Called QUEUE-NEXT on a thread that is already queued."))
    (setf (next-action thread) action)
    (setf *thread-queue*
          (snoc *thread-queue* thread))
    (when (not *current-thread*) (thread-loop))))

(defun queue-next-replace (action &optional thread)
  (let ((thread (or thread *current-thread*)))
    (when (null thread)
      (error "Must provide thread to QUEUE-NEXT-REPLACE if not already in thread."))
    (when (not (next-action thread))
      (error "Called QUEUE-NEXT-REPLACE on a thread that isn't already queued."))
    (setf (next-action thread) action)
    (setf *thread-queue*
          (snoc *thread-queue* thread))
    (when (not *current-thread*) (thread-loop))))

(defun make-thread (function &key name)
  "Create a new green thread with an optional :name. The first parameter
   should be a function that takes no arguments. The thread will be queued
   immediately, so will run immediately if make-thread is called from
   outside any other green threads. Returns a thread object."
  (multiple-value-bind
      (binding-symbols binding-values)
      (bindings-from-alist *default-special-bindings*)
    (let ((new-thread
            (make-instance 'thread
                           :name name
                           :binding-symbols binding-symbols
                           :binding-values binding-values)))
      (push new-thread *active-threads*)
      (queue-next function new-thread)
      new-thread)))

(defun/cc thread-yield ()
  "A convenience function to yield control back to thread loop. Only works
   properly if continuation is available, so must be run within
   WITH-GREEN-THREAD macro or CL-CONT:WITH-CALL/CC transformed code."
  (let/cc continuation
    (queue-next continuation)))

(defmacro with-green-thread (&body body)
  "A convenience macro that runs the code in a lambda wrapped in CPS
   transformin macros and a call to make-thread. Returns thread object."
  `(without-call/cc (make-thread (with-call/cc (lambda () ,@body)))))

;; Some BORDEAUX-THREADS functionality
(defun all-threads ()
  "Returns a list of all active threads, which are threads that have
   been created but not destroyed or finished."
  *active-threads*)

(defun current-thread ()
  "Returns the currently running greed thread or NIL if not called from
   within thread."
  *current-thread*)

(defun threadp (obj)
  "Returns T if the object passed in is a thread."
  (typep obj 'thread))

(defun thread-name (thread)
  "Returns the name of the thread given at the time of creation or NIL."
  (name thread))

(defun destroy-thread (thread)
  "Destroys the thread passed in, so it will not run any more. It is an
   error to call this on the current thread."
  (when (eq thread *current-thread*)
    (error "Called DESTROY-THREAD on self."))
  (setf (alive thread) nil)
  (setf (next-action thread) nil)
  (setf *active-threads*
        (remove-if #'(lambda (other) (eq thread other))
                   *active-threads*))
  (complete-future (join-future thread)))

(defun thread-alive-p (thread)
  "Returns T if the passed in thread has not been destroyed."
  (alive thread))

(defun get-join-future (thread)
  "Gets a future object from thread which will be completed when
   the thread is finished."
  (join-future thread))

(defun/cc join-thread (thread)
  (when (not *current-thread*)
    (error "Called JOIN-THREAD not from within a green thread."))
  (wait-on (join-future thread)))

;; Futures

(defclass green-future (cl-async-future:future)
  ((threads-waiting :initform (make-instance 'batched-queue))))

(defun make-green-future ()
  "Create a future object. A future can be completed once and can
   be signalled with multiple return values at that time."
  ;; explicitely create our future to be thread-centric (no double-fireing)
  (make-instance 'green-future :preserve-callbacks nil :reattach-callbacks t))

(defun queue-future (future action &optional thread)
  "Queues an action on current (or specified) thread to take place when
   provided future is completed."
  (let ((thread (or thread *current-thread*)))
    (when (null thread)
      (error "Must provide thread to queue-future if not already in thread."))
    (when (next-action thread)
      (error "Called queue-future on a thread that is already queued."))
    (with-slots ((completep finished) threads-waiting) future
      (if (not completep)
        (progn
          (setf (next-action thread) action)
          (setf threads-waiting (snoc threads-waiting thread)))
        (queue-next action thread)))))

(defmethod finish ((future green-future) &rest values)
  (with-slots ((completep finished) threads-waiting) future
    (when completep
      (error "Called COMPLETE-FUTURE on an already completed future."))
    ;; let cl-async-future do its magic
    (call-next-method)
    (loop while (not (empty-p threads-waiting))
          do (let ((next-thread (head threads-waiting)))
               (setf threads-waiting (tail threads-waiting))
               (setf *thread-queue* (snoc *thread-queue* next-thread))))
    (when (not *current-thread*) (thread-loop))))

(defun complete-future (future &rest values)
  "Signals that a future is complete and provides return values for the
   future."
  (apply 'finish (append (list future) values)))

(defun future-complete-p (future)
  "Returns T if the future has been completed."
  (slot-value future 'finished))

(defun future-values (future)
  "Returns the values given to the future when it
   was completed."
  (values-list (cl-async-future::future-values future)))

(defun/cc wait-on (future)
  "Requires CL-CONT:WITH-CALL/CC environment, causes the current 
   thread to wait for the completion of the specified future and 
   returns the values given to the future when it was completed."
  (let/cc continuation
    (queue-future future continuation))
  (future-values future))

;; channels

(defun/cc send/cc (channel value &key (blockp t))
  "Requires CL-CONT:WITH-CALL/CC environment. Sends a value to a
   channel, and blocks, unless blockp is nil. Returns the channel
   that received the value unless blockp is nil and there is no
   thread waiting to receive in which case it returns nil."
  (let/cc continuation
    (send channel value continuation :blockp blockp)))

(defun/cc recv/cc (channel &key (blockp t))
  "Requires CL-CONT:WITH-CALL/CC environment. Receives a value from a 
   channel, and blocks, unless blockp is nil. Returns 2 values, the
   first is the value being received and the second is a generalized
   boolean that is only nil if blockp is nil and there is no thread
   waiting to send a value."
  (let/cc continuation
    (recv channel continuation :blockp blockp)))

(defgeneric send (channel value continuation &key))
(defgeneric recv (channel continuation &key))

(defclass channel ()
  ((waiting-to-send :initform (make-instance 'batched-queue))
   (waiting-to-recv :initform (make-instance 'batched-queue))))

(defmethod send ((channel channel) value continuation &key (blockp t))
  (with-slots (waiting-to-send waiting-to-recv) channel
    (if (empty-p waiting-to-recv)
      (if blockp
        (with-slots (next-action) *current-thread*
          (setf next-action continuation)
          (setf waiting-to-send
                (snoc waiting-to-send
                      (list *current-thread* value))))
        (queue-next (lambda () (funcall continuation nil))))
      (let ((recv-thread (head waiting-to-recv))) 
        (setf waiting-to-recv (tail waiting-to-recv))
        (queue-next-replace
          (let ((action (next-action recv-thread)))
            (lambda () (funcall action value t)))
          recv-thread)
        (queue-next (lambda () (funcall continuation channel)))))))

(defmethod recv ((channel channel) continuation &key (blockp t))
  (with-slots (waiting-to-send waiting-to-recv) channel
    (if (empty-p waiting-to-send)
      (if blockp
        (with-slots (next-action) *current-thread*
          (setf next-action continuation)
          (setf waiting-to-recv
               (snoc waiting-to-recv *current-thread*)))
        (queue-next (lambda () (funcall continuation nil nil))))
      (destructuring-bind (send-thread value) (head waiting-to-send)
        (setf waiting-to-send (tail waiting-to-send))
        (queue-next-replace
          (let ((action (next-action send-thread)))
            (lambda () (funcall action channel))) 
          send-thread)
        (queue-next (lambda () (funcall continuation value t)))))))
