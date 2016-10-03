(in-package :cl-user)

(defpackage :mock-server
  (:use :cl)
  (:export :start
	   :stop
	   :dump-logs
           :*address*
           :*continuously-running*))

(in-package :mock-server)

(defparameter *httpd* nil)

(defparameter *output-stream* (make-string-output-stream))

(defparameter *port* 8082)

(defparameter *address* (format nil "http://localhost:~A" *port*))

;; (defvar *continuously-running* nil)
(setf hunchentoot:*dispatch-table*
      `(,(hunchentoot:create-prefix-dispatcher "" 'hello-page)))

(defun hello-page ()
  "<html><body>Hello World!</body></html>")

(defun start ()
  (setf *httpd* (hunchentoot:start
                 (make-instance 'hunchentoot:acceptor
                                :port *port*
                                :access-log-destination *output-stream*))))

(defun stop ()
  (hunchentoot:stop *httpd*)
  (setf *httpd* nil)
  (print "Mock server stoped."))

(defun dump-logs ()
  (get-output-stream-string *output-stream*))

