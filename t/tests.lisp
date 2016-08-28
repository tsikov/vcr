(in-package :cl-user)

(defpackage vcr-test
  (:use :cl
	:prove
	:mock-caveman2-app
	:vcr))

(in-package :vcr-test)

(setf prove:*enable-colors* t)

(defun prepare-tests ()
  ;; start clack
  (mock-caveman2-app:start :port 8080)
  ;; set *shelf* to random non-existing directory
  (setf *shelf*	(concatenate 'string
			     "/tmp/"
			     (write-to-string (get-universal-time))
			     "/")))

(defun cleanup ()
  ;; stop clack
  (mock-caveman2-app:stop)
  ;; remove directory
  (delete-file (tape-path "testing"))
  (uiop:delete-empty-directory *shelf*))

(defun run-tests ()

  (plan 1)

  (subtest "Test creation of shelf and cassette."
    (is (probe-file *shelf*) nil
	"Shelf is not available at first.")
    (with-vcr "testing"
      (drakma:http-request "http://example.com")
      )
    (isnt (probe-file *shelf*) nil
	  "After http request is made shelf is created."))

  (finalize))

(prepare-tests)
(run-tests)
(cleanup)
