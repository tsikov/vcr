(in-package :cl-user)
(defpackage mock-caveman2-app-test-asd
  (:use :cl :asdf))
(in-package :mock-caveman2-app-test-asd)

(defsystem mock-caveman2-app-test
  :author ""
  :license ""
  :depends-on (:mock-caveman2-app
               :prove)
  :components ((:module "t"
                :components
                ((:file "mock-caveman2-app"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
