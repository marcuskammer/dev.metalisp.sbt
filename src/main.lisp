;;;; -*- mode: lisp; coding: utf-8; tab-width: 4; fill-column: 100; indent-tabs-mode: nil; -*-
;;;; main - Provide general functions.

(defpackage dev.metalisp.sbt
  (:use :cl)
  (:export
   :*use-cdn*
   :*cdn-css-url*
   :*cdn-js-url*
   :*bs-version*
   :*color-theme*
   :write-html-to-file
   :with-page))

(in-package :dev.metalisp.sbt)

(setq spinneret:*fill-column* 120)

(defparameter *bs-version* "5.3.2")

(defparameter *use-cdn* t)

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

(defparameter *local-css-url*
  "5.3.0/bootstrap.min.css")

(defparameter *local-js-url*
  "5.3.0/bootstrap.bundle.min.js")

(defparameter *color-theme* "dark")

(defun css-url ()
  (if *use-cdn*
      *cdn-css-url*
      *local-css-url*))

(defun js-url ()
  (if *use-cdn*
      *cdn-js-url*
      *local-js-url*))

(defun download-file (url directory)
  "Downloads a file from a given URL and saves it to the specified directory."
  (let* ((filename (car (last (uiop:split-string url :separator "/"))))
         (filepath (merge-pathnames filename directory)))
    (ensure-directories-exist directory)
    (with-open-file (stream filepath
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (dex:get url :stream stream))
    filepath))

(defun write-html-str-to-file (filename string &key (lang "en") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string string stream))))

(defmacro with-page ((&key meta title add-css-urls add-js-urls) &body body)
  (unless title
    (error "Please add a title"))
  `(spinneret:with-html
     (:doctype)
     (:html :data-bs-theme ,*color-theme*
            (:head (:meta :charset "utf-8")
                   (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                   ,@(loop for (key value) on meta by #'cddr
                           collect `(:meta :name
                                           ,(string-downcase (symbol-name key))
                                           :content ,(getf meta key)))

                   (:title ,title)

                   (:link :type "text/css" :rel "stylesheet" :href ,(css-url))
                   ,@(loop for url in add-css-urls
                           collect `(:link :type "text/css" :rel "stylesheet" :href ,url)))

            (:body (:h1 :class "visually-hidden" ,title)
              (:main ,@body)

              (:script :src ,(js-url))
              ,@(loop for url in add-js-urls
                      collect `(:script :src ,url))))))
