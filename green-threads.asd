#|
  This file is a part of green-threads project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

#|
  Author: Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage green-threads-asd
  (:use :cl :asdf))
(in-package :green-threads-asd)

(defsystem green-threads
  :version "0.2"
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:cl-cont :cl-async-future)
  :components ((:module "src"
                :components
                ((:file "green-threads"))))
  :description "A lightweight threading / cooperative multitasking library."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op green-threads-test))))
