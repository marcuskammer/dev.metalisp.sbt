;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-sbt/section
  (:use :cl)
  (:export :with-section
   :with-title-bar))

(in-package :ml-sbt/section)

(defmacro with-title-bar (head &rest items)
  "Creates a Bootstrap-styled title bar with an optional set of action buttons.

HEAD: The text for the title bar's heading.

ITEMS: An optional list of alternating label and URL pairs for action buttons.

Example:
  (with-title-bar \"My Section\" \"Add\" \"/add\" \"Save\" \"/save\")"
  `(spinneret:with-html
     (:div :class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"
           (:h* ,head)
           ,@(when items
               `(:div :class "btn-group" :role "group"
                      ,@(loop for (label url) on items by #'cddr
                              collect `(:a :class "btn btn-outline-primary"
                                           :href ,url
                                           ,label)))))))

(defmacro with-section (&body body)
  "Creates a Bootstrap-styled section.

BODY: The content of the section, typically including a call to with-title-bar.

Example:
  (with-section
    (with-title-bar \"My Section\" \"Add\" \"/add\" \"Save\" \"/save\")
    (:p \"Section content goes here\"))"
  `(spinneret:with-html
     (:section :class "mb-3"
               ,@body)))
