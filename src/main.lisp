(defpackage cl-sbt
  (:use :cl)
  (:export
   :container
   :col-md
   :col-xs
   :col-sm
   :col-lg
   :icon
   :well
   :with-page))

(in-package :cl-sbt)

(defmacro container ((&key (fluid nil)) &body body)
  "Bootstrap Grid Container. Set :fluid t for fluid layout."
  `(spinneret:with-html
     (:div :class ,(if fluid "container-fluid" "container")
           ,@body)))

(defmacro row (&body body)
  "Use rows to create horizontal groups of columns."
  `(spinneret:with-html
     (:div :class "row"
           ,@body)))

(defmacro icon ((&key (glyph "asterisk")))
  "Glyphicon from bootstrap"
  `(spinneret:with-html
     (:span :class (format nil "glyphicon glyphicon-~a" ,glyph))))

(defmacro well (&body body)
  "bootstrap well"
  `(spinneret:with-html
     (:div :class "well"
           ,@body)))
