(in-package :cl-user)

(defpackage vcr-test
  (:use :cl
	:prove
	:mock-server
	:vcr))

(in-package :vcr-test)

;; Make prove print fabulous reports
(setf prove:*enable-colors* t)

(defparameter *testing-tape-name* "testing")
(defparameter *testing-tape-name-dexador* "testing-dexador")

(defun prepare-tests ()
  ;; start mock server
  (mock-server:start)
  ;; set *shelf* to random non-existing directory
  (setf *shelf*
        (format nil "/tmp/~A/" (write-to-string
                                (get-universal-time)))))

(defun simple-mock-request ()
  (with-vcr *testing-tape-name*
    (drakma:http-request mock-server:*address*)))

(defun simple-mock-request-dexador ()
  (with-vcr *testing-tape-name*
    (dex:get mock-server:*address*)))

(defun delete-file-if-exists (filepath)
  (if (probe-file filepath)
      (delete-file filepath)))

(defun cleanup ()
  ;; stop mock server
  (mock-server:stop)
  ;; remove tapes if they exists
  (delete-file-if-exists (tape-path *testing-tape-name*))
  (delete-file-if-exists (tape-path *testing-tape-name-dexador*))
  ;; remove directory
  (uiop:delete-empty-directory *shelf*)
  (setf vcr:*original-fn-symbol* 'drakma:http-request)
  (print "Cleanup completed."))

(defun run-tests ()
  
  (subtest "Shelf and tape are created if non-existent."
    (is (probe-file *shelf*) nil
	"Shelf is not available at first.")
    (simple-mock-request)
    (isnt (probe-file *shelf*) nil
	  "After http request is made shelf is created.")
    (isnt (probe-file (tape-path *testing-tape-name*)) nil
	  "After http request is made tape is created."))

  (subtest "Repeated requests don't reach the server."
      ;; first request is already made from the previous test
      (isnt (length (mock-server:dump-logs)) 0
	    "A request is made to the server the first time a page is visited.")
      ;; second request
      (simple-mock-request)
      (is (length (mock-server:dump-logs)) 0
	  "The second request doesn't reach the server as it is cached."))

  (subtest "The contents of the tape are the same as the ones returned by drakma."
    (let ((drakma-response
           (multiple-value-bind (content code headers)
               (drakma:http-request mock-server:*address*)
             (list content code headers)))
          (vcr-response (cadar (read-tape *testing-tape-name*))))
      (is drakma-response vcr-response
  	  "The contents of the drakma response is the same as the tape."
          :test #'equalp)
      (is (apply #'values drakma-response)
          (simple-mock-request)
          "VCR returns the results from the cache as multiple values and not as a list")))

  (subtest "Turn on the ability to print to a designated stream"
    (is-print (simple-mock-request) ""
              "Logging is turned off by default")
    (setf *log-output* *standard-output*)
    (is-print (simple-mock-request) ""
              "But can be turned on")
    ;; let's turn it off again
    (setf *log-output* nil))
                                     
  (subtest "Change request function to use a different library."
    (setf vcr:*original-fn-symbol* 'dex:get)
    (let ((dexador-response (dex:get mock-server:*address*)))
      (is (simple-mock-request-dexador) dexador-response
          "Can use dexador as a request library"))))

(prepare-tests)
(plan 5)
(run-tests)
(finalize)
(cleanup)
