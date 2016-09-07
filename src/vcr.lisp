(in-package #:vcr)

;; The symbol original-fn is internal for the package so
;; no name conflict is possible.
(setf (symbol-function 'original-fn) #'drakma:http-request)

(defvar *shelf* "/tmp/vcr/")

;; helper functions

(defun tape-path (tape)
  ;; The following line is needed, because *shelf*
  ;; can be changed dynamically.
  (ensure-directories-exist *shelf*)
  ;; coerce pathnames to string so they can be concatenated
  (setf *shelf* (namestring *shelf*))
  (concatenate 'string *shelf* tape ".lisp"))

(defun read-tape (tape)
  "Returns the contents of the file (tape) if found or
   returns nil if not."
  (let ((tape-path (tape-path tape)))
    (if (probe-file tape-path)
	(with-open-file (stream
		         tape-path
		         :direction :input)
	  (read stream)))))

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
  (rest
   (find-if #'(lambda (el) (equal args (car el))) cache)))

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
	     (let* ((cached-result (get-cached-result args cache)))
	       (if cached-result
		   cached-result
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

                     ;; Return the computed result. (If we didn't have
                     ;; put it there the whole cache would have been
                     ;; returned instead)
		     computed-result)))))

     ;; PROG1 is used, to return the result of the ,@body
     ;; as after finishing the work we need to set back
     ;; the drakma:http-request function to it's original
     ;; unmemoized version. This way users of the library
     ;; can continue using drakma:http-request as usual
     ;; outside the lexical environment created by the with-vcr
     ;; clojure.
     (prog1
	 (progn ,@body)
       (setf (symbol-function 'drakma:http-request)
	     #'original-fn)
       (record-tape cache ,tape))))
