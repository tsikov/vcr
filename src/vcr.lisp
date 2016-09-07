(in-package #:vcr)

(setf (symbol-function 'original-fn) #'drakma:http-request)

(defvar *shelf* "/tmp/vcr/")

(defun tape-path (tape)
  ;; The following line is needed, because *shelf*
  ;; can be changed dynamically.
  (ensure-directories-exist *shelf*)
  (concatenate 'string *shelf* tape ".lisp"))

(defun read-tape (tape)
  (let ((tape-path (tape-path tape)))
    (if (probe-file tape-path)
	(with-open-file (stream
		         tape-path
		         :direction :input)
	  (read stream)))))

(defun store-in-cache (args result cache)
  (acons args (list result) cache))

(defun get-cached-result (args cache)
  (rest
   (find-if #'(lambda (el) (equal args (car el))) cache)))

(defun record-tape (cache tape)
  (with-open-file (stream (tape-path tape)
			  :direction :output
			  :if-exists :supersede)
    (print cache stream)))

(defmacro with-vcr (tape &body body)
  `(let ((cache (read-tape ,tape)))
     (setf (symbol-function 'drakma:http-request)
	   (lambda (&rest args)
	     (let* ((cached-result (get-cached-result args cache)))
	       (if cached-result
		   cached-result
		   (let ((computed-result
                          (multiple-value-bind (content code headers)
                                (apply #'original-fn args)
                            (list content code headers))))
		     (setf cache
			   (store-in-cache args computed-result cache))
		     computed-result)))))
     (prog1
	 (progn ,@body)
       (setf (symbol-function 'drakma:http-request)
	     #'original-fn)
       (record-tape cache ,tape))))
