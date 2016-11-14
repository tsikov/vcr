;;;; package.lisp

(defpackage #:vcr
  (:use #:cl)
  (:export #:*shelf*
           #:*log-output*
           #:*url-only-cache-hits*
	   #:tape-path ; not really needed, but can be useful
           #:read-tape ; not really needed, but can be useful
	   #:with-vcr))

