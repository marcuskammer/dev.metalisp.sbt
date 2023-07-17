(defpackage cl-sbt
  (:use :cl)
  (:export
   :write-string-to-file
   :with-page))

(in-package :cl-sbt)

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet" :href "public/5.3.0/bootstrap.min.css"))
      (:body ,@body))))

(defun write-string-to-file (filename string)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string string stream)))
