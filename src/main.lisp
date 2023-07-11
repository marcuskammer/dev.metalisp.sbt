(defpackage cl-sbt
  (:use :cl)
  (:export :badge :badge-pill-primary :badge-pill-secondary :badge-pill-success :badge-pill-danger :badge-pill-warning :badge-pill-info :badge-pill-light :badge-pill-dark :btn :btn-primary :btn-secondary :btn-success :btn-danger :btn-warning :btn-info :btn-light :btn-dark :btn-link :btn-outline-primary :btn-outline-secondary :btn-outline-success :btn-outline-danger :btn-outline-warning :btn-outline-info :btn-outline-light :btn-outline-dark :btn-outline-link :container :col-md :col-xs :col-sm :col-lg :icon :dropdown :dropdown-menu :dropdown-item :nav :nav-item :pagination :pagination-with-icons :row :table :table-striped :table-bordered :table-hover :table-condensed :well :with-page))

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
