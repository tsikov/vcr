;;;; vcr.asd

(asdf:defsystem #:vcr
  :description "Recordings"
  :author "Petko Tsikov <tsikov@gmail.com>"
  :license "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "vcr"))
  :depends-on (:drakma))
