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

(defmacro col-md ((&key (grid 12) (offset 0)) &body body)
  "Medium  device grid columns >= 992 px"
  `(spinneret:with-html
     (:div :class (format nil "col-md-~d col-md-offset-~d" ,grid ,offset)
           ,@body)))

(defmacro col-xs ((&key (grid 12) (offset 0)) &body body)
  "Extra small device grid columns, Phones < 768 px"
  `(spinneret:with-html
     (:div :class (format nil "col-xs-~d col-xs-offset-~d" ,grid ,offset)
           ,@body)))

(defmacro col-sm ((&key (grid 12) (offset 0)) &body body)
  "Small device grid columns, Tablets >= 768 px"
  `(spinneret:with-html
     (:div :class (format nil "col-sm-~d col-sm-offset-~d" ,grid ,offset)
           ,@body)))

(defmacro col-lg ((&key (grid 12) (offset 0)) &body body)
  "Large devices , Desktops >= 1200 px"
  `(spinneret:with-html
     (:div :class (format nil "col-lg-~d col-lg-offset-~d" ,grid ,offset)
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
