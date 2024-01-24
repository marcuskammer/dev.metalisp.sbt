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

(defun write-html-to-file (filename string &key (lang "en") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (write-string string stream))))
