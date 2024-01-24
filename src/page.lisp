;;;; -*- mode: lisp; coding: utf-8; tab-width: 4; fill-column: 100; indent-tabs-mode: nil; -*-
;;;; page - Provide macros for different type of pages

(defpackage dev.metalisp.sbt/page
  (:use :cl)
  (:export
   :with-landing-page
   :with-blogpost-page
   :with-product-page
   :with-contact-page
   :with-portfolio-page
   :with-error-page
   :with-login-page
   :with-dashboard-page
   :with-about-page
   :with-faq-page
   :with-tos-page))

(in-package :dev.metalisp.sbt/page)

(defparameter *bs-version* "5.3.2")
(defparameter *cdn-css-url*
  (concatenate 'string
               "https://cdn.jsdelivr.net/npm/bootstrap@"
               *bs-version*
               "/dist/css/bootstrap.min.css"))
(defparameter *cdn-js-url*
  (concatenate 'string
               "https://cdn.jsdelivr.net/npm/bootstrap@"
               *bs-version*
               "/dist/js/bootstrap.bundle.min.js"))

(defun get-cdn-css
    (if *use-cdn*
        (:link :type "text/css" :rel "stylesheet" :href *cdn-css-url*)
        (:link :type "text/css" :rel "stylesheet" :href "5.3.0/bootstrap.min.css")))

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
            (:body (:h1 :class "visually-hidden" ,pagetitle)
              (:main ,@body))
            (if ,cdn
                (:script :src *cdn-js*)
                (:script :src "5.3.0/bootstrap.bundle.min.js")))))

(defmacro with-landing-page)
(defmacro with-blogpost-page)
(defmacro with-product-page)
(defmacro with-contact-page)
(defmacro with-portfolio-page)
(defmacro with-error-page)
(defmacro with-login-page)
(defmacro with-dashboard-page)
(defmacro with-about-page)
(defmacro with-faq-page)
(defmacro with-tos-page)
