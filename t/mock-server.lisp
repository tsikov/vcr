(in-package :cl-user)

(defpackage :mock-server
  (:use :cl)
  (:export :start
	   :stop))

(in-package :mock-server)

(defparameter *httpd* nil)

(defparameter *port* 8080)

(defun start ()
  (setf *httpd*
    (hunchentoot:start
     (make-instance 'hunchentoot:acceptor
		    :port *port*)))
  (format t "Hunchentoot started on port ~d" *port*))

(defun stop ()
  (hunchentoot:stop *httpd*)
  (print "Hunchentoot stoped."))

;; (hunchentoot:define-easy-handler (hello-world (:uri "/hello"))
;;     ()
;;   (with-html-output (*standard-output* nil :indent t)
;;        (:html
;;               (:head
;;                  (:title "Hello World"))
;;               (:body
;;                  (:p "Hello world!...")))))
