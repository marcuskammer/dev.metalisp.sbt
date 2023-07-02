(in-package :cl-sbt)

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))
