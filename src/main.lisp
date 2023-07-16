(defpackage cl-sbt
  (:use :cl)
  (:export
   :write-string-to-file))

(in-package :cl-sbt)

(defun write-string-to-file (filename string)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string string stream)))
