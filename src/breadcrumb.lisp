;;;; -*- mode: common-lisp; coding: utf-8; -*-

(defpackage ml-sbt/breadcrumb
  (:use :cl)
  (:export :with-breadcrumb))

(defmacro with-breadcrumb (&rest items)
  "Creates a Bootstrap breadcrumb navigation.

ITEMS: A list of (label . url) pairs. The last item is automatically set as active.

Example usage:
  (with-breadcrumb
    (\"Home\" . \"/\")
    (\"Library\" . \"/library\")
    (\"Data\" . nil))"
  `(spinneret:with-html
     (:nav :class "container"
           :aria-label "breadcrumb"
           (:ol :class "breadcrumb"
                ,@(loop for (label url) on items by #'cddr
                        for last-p = (null (cdr items))
                        collect (if last-p
                                    `(:li :class "breadcrumb-item active" :aria-current "page" ,label)
                                    `(:li :class "breadcrumb-item" (:a :href ,url ,label))))))))
