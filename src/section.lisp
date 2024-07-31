;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-sbt/section
  (:use :cl)
  (:export :with-section))

(in-package :ml-sbt/section)

(defmacro with-section (headline &body body)
  `(spinneret:with-html
     (:section :class "mb-3"
               (:div :class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"
                     (:h* ,headline))
               ,@body)))
