;;;; -*- mode: lisp; coding: utf-8; tab-width: 4; fill-column: 100; indent-tabs-mode: nil; -*-
;;;; page - Provide macros for different types of pages

(defpackage dev.metalisp.sbt/page
  (:use :cl)
  (:import-from
   :dev.metalisp.sbt
   :*color-themes*
   :*use-cdn*
   :*cdn-css-url*
   :*cdn-js-url*)
  (:export
   :with-page))

(in-package :dev.metalisp.sbt/page)

(defmacro with-page ((&key pagemeta pagetitle pagecss pagejs) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html :data-bs-theme ,*color-theme*
            (:head
             (:meta :charset "utf-8")
             (:meta :name "viewport" :content "width=device-width, initial-scale=1")
             ,@(loop for (key value) on pagemeta by #'cddr
                     collect `(:meta :name ,(string-downcase (symbol-name key)) :content ,(getf pagemeta key)))
             (:title ,pagetitle)
             (if *use-cdn*
                 (:link :type "text/css" :rel "stylesheet" :href ,*cdn-css-url*)
                 (:link :type "text/css" :rel "stylesheet" :href "5.3.0/bootstrap.min.css"))
             ,@(loop for css in pagecss
                     collect `(:link :type "text/css" :rel "stylesheet" :href ,css))
             (:body (:h1 :class "visually-hidden" ,pagetitle)
               (:main ,@body))
             (if ,*use-cdn*
                 (:script :src *cdn-js-url*)
                 (:script :src "5.3.0/bootstrap.bundle.min.js"))
             ,@(loop for js in pagejs
                     collect `(:script :src ,js))))))
