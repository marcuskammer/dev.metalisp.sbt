(defpackage cl-sbt
  (:use :cl)
  (:export
   :write-string-to-file
   :with-page))

(in-package :cl-sbt)

(defparameter *cdn-css* "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css")
(defparameter *cdn-js* "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")

(defmacro with-page ((&key cdn title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title ,title)
       (if ,cdn
           (:link :type "text/css" :rel "stylesheet" :href ,*cdn-css*)
           (:link :type "text/css" :rel "stylesheet" :href "5.3.0/bootstrap.min.css")))
      (:body ,@body)
      (if ,cdn
          (:script :src *cdn-js*)
          (:script :src "5.3.0/bootstrap.bundle.min.js")))))

(defun write-string-to-file (filename string)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string string stream)))
