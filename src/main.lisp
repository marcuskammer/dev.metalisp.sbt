(defpackage cl-sbt
  (:use :cl)
  (:export
   :write-string-to-file
   :with-page))

(in-package :cl-sbt)

(defmacro with-page ((&key cdn title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title ,cdn)
       (if ,cdn
           (:link :type "text/css" :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css")
           (:link :type "text/css" :rel "stylesheet" :href "5.3.0/bootstrap.min.css")))
      (:body ,@body)
      (if ,cdn
          (:script :src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")
          (:script :src "5.3.0/bootstrap.bundle.min.js")))))

(defun write-string-to-file (filename string)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string string stream)))
