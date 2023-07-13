(in-package :cl-sbt-album)

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))
