;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-sbt/section
  (:use :cl)
  (:export :with-section))

(in-package :ml-sbt/section)

(defmacro with-section ((headline &optional functions) &body body)
  "Creates a Bootstrap-styled section with a headline and optional function buttons.

HEADLINE: The text for the section's headline.

FUNCTIONS: An optional list of alternating label and URL pairs for function buttons.

BODY: The content of the section.

Example:
  (with-section (\"My Section\" (\"Add\" \"/add\" \"Save\" \"/save\"))
    \"Section content goes here\")"
  `(spinneret:with-html
     (:section :class "mb-3"
               (:div :class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"
                     (:h* ,headline)
                     ,@(loop for (label url) on functions by #'cddr
                             when functions
                               collect `(:a :class "btn btn-outline-primary"
                                            :href ,url
                                            ,label)))
               ,@body)))
