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
           ;; #:join-thread ;; TODO

           ;; additional:
           #:queue-thread
           #:thread-yield
           #:with-green-thread))
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
(defparameter *waiting-threads* (make-instance 'batched-queue))
(defparameter *future-threads* nil)

;; classes
(defclass thread ()
  ((name :initarg :name :reader name)
   (binding-symbols :initarg :binding-symbols :reader binding-symbols)
   (binding-values :initarg :binding-values :reader binding-values)
   (next-action :initform nil :accessor next-action)
   (alive :initform T :accessor alive)))

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
                        *active-threads*)))

(defun thread-loop ()
  (loop while (not (empty-p *waiting-threads*))
        do (let ((thread (head *waiting-threads*)))
             (setf *waiting-threads* (tail *waiting-threads*))
             (when (alive thread) ;; destroyed threads might be here
               (progv (binding-symbols thread) (binding-values thread)
                 (let ((*current-thread* thread)
                       (action (next-action thread)))
                   (setf (next-action thread) nil)
                   (funcall action)
                   (if (not (next-action thread))
                     (deactivate-thread thread))))))))

(defun queue-thread (thread action)
  (when (next-action thread)
    (error "Called queue-thread on a thread that is already queued."))
  (setf (next-action thread) action)
  (setf *waiting-threads*
        (snoc *waiting-threads* thread))
  (when (not *current-thread*) (thread-loop)))

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
      (queue-thread new-thread function))))

(defun/cc thread-yield ()
  (let/cc continuation
    (queue-thread *current-thread* continuation)))

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

;(defun special-binding-example ()
  ;(let ((*default-special-bindings* '((*test-special* . "foo"))))
    ;(with-green-thread
      ;(format t "1: ~a~%" *test-special*)
      ;(let ((*default-special-bindings* '((*test-special* . "bar"))))
        ;(with-green-thread (format t "3: ~a~%" *test-special*)))
      ;(format t "2: ~a~%" *test-special*))))
