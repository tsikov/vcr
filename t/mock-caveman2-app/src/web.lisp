(in-package :cl-user)
(defpackage mock-caveman2-app.web
  (:use :cl
        :caveman2
        :mock-caveman2-app.config
        :mock-caveman2-app.view
        :mock-caveman2-app.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :mock-caveman2-app.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
