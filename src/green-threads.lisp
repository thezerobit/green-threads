#|
  This file is a part of green-threads project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage green-threads
  (:use :cl :cl-cont)
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

           #:thread-yield

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
           #:make-future
           #:queue-future
           #:complete-future
           #:future-complete-p
           #:wait-for))
(in-package :green-threads)

;; Batched-Queue
;; adapted from "Purely Function Data Structures" by Chris Okasaki

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
   (join-future :initform nil)))

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

(defun deactivate-thread (thread)
  (setf *active-threads*
        (remove-if (lambda (other) (eq thread other))
                        *active-threads*))
  (with-slots (join-future) thread
    (when join-future (complete-future join-future))))

(defun thread-loop ()
  (loop while (not (empty-p *thread-queue*))
        do (let ((thread (head *thread-queue*)))
             (setf *thread-queue* (tail *thread-queue*))
             (when (alive thread) ;; destroyed threads might be here
               (progv (binding-symbols thread) (binding-values thread)
                 (let ((*current-thread* thread)
                       (action (next-action thread)))
                   (setf (next-action thread) nil)
                   (funcall action)
                   (if (not (next-action thread))
                     (deactivate-thread thread))))))))

(defun queue-next (action &optional thread)
  (let ((thread (or thread *current-thread*)))
    (when (null thread)
      (error "Must provide thread to queue-next if not already in thread."))
    (when (next-action thread)
      (error "Called queue-next on a thread that is already queued."))
    (setf (next-action thread) action)
    (setf *thread-queue*
          (snoc *thread-queue* thread))
    (when (not *current-thread*) (thread-loop))))

(defun make-thread (function &key name)
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
  (let/cc continuation
    (queue-next continuation)))

(defmacro with-green-thread (&body body)
  `(without-call/cc (make-thread (with-call/cc (lambda () ,@body)))))

;; Some BORDEAUX-THREADS functionality
(defun all-threads () *active-threads*)

(defun current-thread () *current-thread*)

(defun threadp (obj) (typep obj 'thread))

(defun thread-name (thread) (name thread))

(defun destroy-thread (thread)
  (when (eq thread *current-thread*)
    (error "Called DESTROY-THREAD on self."))
  (setf (alive thread) nil)
  (setf (next-action thread) nil)
  (deactivate-thread thread))

(defun thread-alive-p (thread) (alive thread))

(defun/cc join-thread (thread)
  (when (not *current-thread*)
    (error "Called JOIN-THREAD not from within a green thread."))
  (with-slots (join-future) thread
    (when (null join-future)
      (setf join-future (make-future)))
    (wait-for join-future)))

;; Futures

(defclass future ()
  ((vals :accessor vals)
   (completep :initform nil)
   (threads-waiting :initform (make-instance 'batched-queue))))

(defun make-future ()
  (make-instance 'future))

(defun queue-future (future action &optional thread)
  (let ((thread (or thread *current-thread*)))
    (when (null thread)
      (error "Must provide thread to queue-future if not already in thread."))
    (when (next-action thread)
      (error "Called queue-future on a thread that is already queued."))
    (with-slots (completep threads-waiting) future
      (if (not completep)
        (progn
          (setf (next-action thread) action)
          (setf threads-waiting (snoc threads-waiting thread)))
        (queue-next action thread)))))

(defun complete-future (future &rest values)
  (with-slots (vals completep threads-waiting) future
    (when completep
      (error "Called COMPLETE-FUTURE on an already completed future."))
    (setf completep T)
    (setf vals values)
    (loop while (not (empty-p threads-waiting))
          do (let ((next-thread (head threads-waiting)))
               (setf threads-waiting (tail threads-waiting))
               (setf *thread-queue* (snoc *thread-queue* next-thread))))
    (when (not *current-thread*) (thread-loop))))

(defun future-complete-p (future)
  (slot-value future 'completep))

(defun/cc wait-for (future)
  (let/cc continuation
    (queue-future future continuation))
  (values-list (vals future)))

