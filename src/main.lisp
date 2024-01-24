;;;; -*- mode: lisp; coding: utf-8; tab-width: 4; fill-column: 100; indent-tabs-mode: nil; -*-
;;;; main - Provide general functions.

(defpackage dev.metalisp.sbt
  (:use :cl)
  (:export
   :*cdn-css*
   :*cdn-js*
   :write-html-to-file
   :with-page
   :*l10n*
   :find-l10n))

(in-package :dev.metalisp.sbt)

(setq spinneret:*fill-column* 120)
(defparameter *cdn-css* "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css")
(defparameter *cdn-js* "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")

(defmacro with-page ((&key (author "") (description "") (cdn t) (pagetitle "") (theme "dark")) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html :data-bs-theme ,theme
            (:head
             (:meta :charset "utf-8")
             (:meta :name "viewport" :content "width=device-width, initial-scale=1")
             (:meta :name "author" :content ,author)
             (:meta :name "description" :content ,description)
             (:title ,pagetitle)
             (if ,cdn
                 (:link :type "text/css" :rel "stylesheet" :href ,*cdn-css*)
                 (:link :type "text/css" :rel "stylesheet" :href "5.3.0/bootstrap.min.css")))
            (:body (:h1 :class "visually-hidden" ,pagetitle)
              (:main ,@body))
            (if ,cdn
                (:script :src *cdn-js*)
                (:script :src "5.3.0/bootstrap.bundle.min.js")))))

(defun write-html-to-file (filename string &key (lang "en") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string string stream))))
