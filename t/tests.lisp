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

(defun prepare-tests ()
  ;; start mock server
  (mock-server:start)
  ;; set *shelf* to random non-existing directory
  (setf *shelf*	(concatenate 'string
			     "/tmp/"
			     (write-to-string (get-universal-time))
			     "/")))

(defun cleanup ()
  ;; stop mock server
  (mock-server:stop)
  ;; remove tape if exists
  (if (probe-file (tape-path *testing-tape-name*))
      (delete-file (tape-path *testing-tape-name*)))
  ;; remove directory
  (uiop:delete-empty-directory *shelf*)
  (print "Cleanup completed."))

(defun run-tests ()
  
  (subtest "Shelf and cassette are created if non-existent."
    (is (probe-file *shelf*) nil
	"Shelf is not available at first.")
    (with-vcr *testing-tape-name*
      (drakma:http-request (mock-server:address)))
    (isnt (probe-file *shelf*) nil
	  "After http request is made shelf is created.")
    (isnt (probe-file (tape-path *testing-tape-name*)) nil
	  "After http request is made cassette is created."))

  (subtest "Repeated requests don't reach the server."
    (with-vcr *testing-tape-name*
      ;; first request is already made the previous test
      (isnt (length (mock-server:dump-logs)) 0
	    "A request is made to the server the first time a page is visited.")

      ;; second request
      (drakma:http-request (mock-server:address))
      (is (length (mock-server:dump-logs)) 0
	  "The second request doesn't reach the server as it is cached.")))

  (subtest "The contents of the cassette are the same as the ones returned by drakma."
    (let ((drakma-response
           (multiple-value-bind (content code headers)
               (drakma:http-request (mock-server:address))
             (list content code headers)))
          (vcr-response (cadar (read-tape *testing-tape-name*))))
      (is drakma-response vcr-response
  	  "The contents of the drakma response is the same as the tape."))))

(prepare-tests)
(plan 3)
(run-tests)
(finalize)
(cleanup)
