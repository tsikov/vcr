(in-package :cl-user)

(defpackage vcr-test
  (:use :cl
	:prove
	:mock-server
	:vcr))

(in-package :vcr-test)

;; configure prove
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
  ;; remove directory
  (delete-file (tape-path *testing-tape-name*))
  (uiop:delete-empty-directory *shelf*)
  (print "Cleanup completed."))

(defun run-tests ()

  (subtest "Shelf and cassette are created if non-existent."
    (is (probe-file *shelf*) nil
	"Shelf is not available at first.")
    (with-vcr *testing-tape-name*
      (drakma:http-request "http://localhost:8080"))
    (isnt (probe-file *shelf*) nil
	  "After http request is made shelf is created.")
    (isnt (probe-file (tape-path *testing-tape-name*)) nil
	  "After http request is made cassette is created.")))

  ;; (subtest "The results of the http call are cached."
  ;;   (is (

  ;; (subtest "The contents of the cassette are the same as the page visited."
  ;;   (is (probe-file *shelf*) nil
  ;; 	"Shelf is not available at first.")
  ;;   (with-vcr "testing"
  ;;     (drakma:http-request "http://localhost:8080"))
  ;;   (isnt (probe-file *shelf*) nil
  ;; 	  "After http request is made shelf is created.")
  ;;   (isnt (probe-file (tape-path "testing")) nil
  ;; 	  "After http request is made cassette is created.")))

(prepare-tests)
(plan 2)
(run-tests)
(finalize)
(cleanup)
