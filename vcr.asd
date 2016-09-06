;;;; vcr.asd

(asdf:defsystem #:vcr
  :description "Store and replay results of http calls for easier testing of external services"
  :author "Petko Tsikov <tsikov@gmail.com>"
  :license "Public Domain"
  :serial t
  :components ((:module "src"
		:components
		((:file "package")
                 (:file "vcr"))))
  :depends-on (:drakma)
  :in-order-to ((test-op (test-op vcr-test))))

(asdf:defsystem #:vcr-test
  :description "Test the vcr library"
  :depends-on (:vcr
               :prove
	       :hunchentoot)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
		:components
	        ((:file "mock-server")
		 (:test-file "tests"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
