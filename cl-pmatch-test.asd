#|
  This file is a part of cl-pmatch project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-pmatch-test-asd
  (:use :cl :asdf))
(in-package :cl-pmatch-test-asd)

(defsystem cl-pmatch-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:cl-pmatch
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-pmatch"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
