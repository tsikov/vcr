(in-package #:vcr)

;; The symbol original-fn is internal for the package so
;; no name conflict is possible.
(setf (symbol-function 'original-fn) #'drakma:http-request)

(defvar *shelf* "/tmp/"
  "The directory holding the cache (tapes). It is exported,
because users of the library are expected to configure VCR
by changing it. E.g.

(setf vcr:*shelf*
      (asdf:system-relative-pathname :my-app :t/cassettes/))

The default directory is set to /tmp/")

;; (defvar *log-output* (make-synonym-stream '*standard-output*))
;; Logging is turned off by default
(defparameter *log-output* nil)

;; helper functions
(defun tape-path (tape)
  "Retruns the full path of the tape given it's name."
  ;; The following line is needed, because *shelf*
  ;; can be changed dynamically.
  (ensure-directories-exist *shelf*)
  ;; coerce pathnames to string so they can be concatenated
  (setf *shelf* (namestring *shelf*))
  ;; A "lisp" extension will make the contents of the cache
  ;; nicely highlighted by editors
  (format nil "~A~A.lisp" *shelf* tape))

;; TODO: Think of a way to read puri:uri and flexi streams
(defun read-tape (tape)
  "Returns the contents of the file (tape) if found or
returns nil if not."
  (let ((tape-path (tape-path tape)))
    (if (probe-file tape-path)
	(with-open-file (stream
		         tape-path
		         :direction :input)
	  (read stream)))))

;; TODO: Think of a way to record puri:uri and flexi streams
(defun record-tape (cache tape)
  "Write the contents of the cache to a file (tape)."
  (with-open-file (stream (tape-path tape)
			  :direction :output
			  :if-exists :supersede)
    (print cache stream)))

(defun store-in-cache (args result cache)
  "Puts a record in the cache."
  (acons args (list result) cache))

(defun get-cached-result (args cache)
  "Gets a record from the cache without the key."
  (cadr (find-if #'(lambda (el) (equal args (car el))) cache)))

(defun as-values (lst)
  "Transform a list into a list of values."
  (apply #'values lst))

(defun log-msg (msg)
  (format *log-output* msg))

;; Any suggestions how this macro can be simplified and improved
;; are welcomed! :)
(defmacro with-vcr (tape &body body)
  `(let ((cache (read-tape ,tape)))

     ;; The following code creates a memoized version
     ;; of the drakma:http-request function and replaces
     ;; the original function.
     (setf (symbol-function 'drakma:http-request)

           ;; We don't know how many arguments the users of the
           ;; drakma:http-request function will use. Hopefully
           ;; &rest & apply saves the day.
	   (lambda (&rest args)
	     (let ((cached-result (get-cached-result args cache)))
	       (if cached-result
                   (progn
                     (log-msg "VCR: cache HIT")
                     
                     ;; The result of the `drakma:http-request` must
                     ;; be returned as multiple values instead of a list
                     (as-values cached-result))
		   (let ((computed-result

                          ;; Temporary omit the puri:uri and the
                          ;; flexi-streams return values as I am
                          ;; not sure how to handle caching with
                          ;; them.
                          (multiple-value-bind (content code headers)
                                (apply #'original-fn args)
                            (list content code headers))))

                     ;; At this point we have a new computed results so
                     ;; we need to put it inside the cache.
		     (setf cache
			   (store-in-cache args computed-result cache))

                     (log-msg "VCR: cache MISS")

                     ;; Return the computed result. (If we didn't have
                     ;; put it there the whole cache would have been
                     ;; returned instead)
		     (as-values computed-result))))))

     ;; PROG1 is used, to return the result of the ,@body
     ;; as after finishing the work we need to set back
     ;; the drakma:http-request function to it's original
     ;; unmemoized version. This way users of the library
     ;; can continue using drakma:http-request as usual
     ;; outside the lexical environment created by the with-vcr
     ;; closure.
     (prog1
	 (progn ,@body)
       (setf (symbol-function 'drakma:http-request)
	     #'original-fn)
       (record-tape cache ,tape))))
