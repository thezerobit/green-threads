#|
  This file is a part of green-threads project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage green-threads-test-asd
  (:use :cl :asdf))
(in-package :green-threads-test-asd)

(defsystem green-threads-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:green-threads
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "green-threads"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
