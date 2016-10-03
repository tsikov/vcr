(in-package :cl-user)

(defpackage :mock-server
  (:use :cl)
  (:export :start
	   :stop
	   :dump-logs
           :*address*))

(in-package :mock-server)

(defvar *httpd* nil)

(defparameter *output-stream* (make-string-output-stream))

(defparameter *port* 8082)

(defparameter *address* (format nil "http://localhost:~A" *port*))

(setf hunchentoot:*dispatch-table*
      `(,(hunchentoot:create-prefix-dispatcher "" 'hello-page)))

(defun hello-page ()
  "<html><body>Hello World!</body></html>")

(defun start ()
  (setf *httpd* (make-instance 'hunchentoot:easy-acceptor
                               :port *port*
                               :access-log-destination
                               *output-stream*))
  (hunchentoot:start *httpd*))

(defun stop ()
  (hunchentoot:stop *httpd*)
  (print "Mock server stoped."))

(defun dump-logs ()
  (get-output-stream-string *output-stream*))
