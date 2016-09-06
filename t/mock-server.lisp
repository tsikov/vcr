(in-package :cl-user)

(defpackage :mock-server
  (:use :cl)
  (:export :start
	   :stop
	   :dump-logs
           :address
           :*continuously-running*))

(in-package :mock-server)

(defparameter *httpd* nil)

(defparameter *output-stream* (make-string-output-stream))

(defparameter *port* 8082)

;; (defvar *continuously-running* nil)

(defun start ()
  (setf *httpd*
        (hunchentoot:start
         (make-instance 'hunchentoot:acceptor
                        :port *port*
                        :access-log-destination *output-stream*))))

(defun stop ()
  (hunchentoot:stop *httpd*)
  (setf *httpd* nil)
  (print "Mock server stoped."))

(defun dump-logs ()
  (get-output-stream-string *output-stream*))

(defun address ()
  (concatenate 'string
               "http://localhost:"
               (write-to-string *port*)))
