(defpackage ml-sbt/tbl
  (:use :cl)
  (:export
   :table))

(in-package :ml-sbt/tbl)

(defmacro table (&body body)
  `(spinneret:with-html
     (:table :class "table"
	     ,@body)))

(defmacro table-striped (&body body)
  `(spinneret:with-html
     (:table :class "table table-striped"
	     ,@body)))

(defmacro table-bordered (&body body)
  `(spinneret:with-html
     (:table :class "table table-bordered"
	     ,@body)))

(defmacro table-hover (&body body)
  `(spinneret:with-html
     (:table :class "table table-hover"
	     ,@body)))

(defmacro table-condensed (&body body)
  `(spinneret:with-html
     (:table :class "table table-condensed"
	     ,@body)))
