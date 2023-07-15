(defpackage cl-sbt-grid
  (:use :cl)
  (:export
   :col
   :col-md
   :col-xs
   :col-sm
   :col-lg))

(in-package :cl-sbt-grid)

(defmacro col ((&key
                  (xs nil)
                  (sm nil)
                  (md nil)
                  (lg nil)
                  (xl nil)
                  (xxl nil)) &body body)
  `(spinneret:with-html
         (:div :class
           ,(concatenate 'string
               (if (null xs) "" (format nil "col-xs-~d" xs))
               (if (null sm) "" (format nil " col-sm-~d" sm))
               (if (null md) "" (format nil " col-md-~d" md))
               (if (null lg) "" (format nil " col-lg-~d" lg))
               (if (null xl) "" (format nil " col-xl-~d" xl))
               (if (null xxl) "" (format nil " col-xxl-~d" xxl)))
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
