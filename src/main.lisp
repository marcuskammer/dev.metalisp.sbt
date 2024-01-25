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

(defparameter *color-theme* "dark")

(defun download-file (url directory)
  "Downloads a file from a given URL and saves it to the specified directory."
  (let* ((filename (car (last (uiop:split-string url :separator "/"))))
         (filepath (merge-pathnames filename directory)))
    (uiop:ensure-all-directories-exist directory)
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
