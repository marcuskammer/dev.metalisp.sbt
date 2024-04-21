;;;; -*- mode: lisp; coding: utf-8; tab-width: 4; fill-column: 100; indent-tabs-mode: nil; -*-
;;;; accordion.lisp
;;;; This file defines a package for generating Bootstrap accordion components using Common Lisp macros.
(defpackage dev.metalisp.sbt/accordion
  (:documentation "A package for generating Bootstrap accordions.")
  (:use :cl)
  (:export
   :accordion))

(in-package :dev.metalisp.sbt/accordion)

(defmacro accordion ((&key (id "accordionExample") flush) &body body)
  "This macro generates an accordion-style collapsible list.

ID: Specifies a unique identifier for the accordion. Defaults to 'accordionExample'.

FLUSH: If t, adds class 'accordion-flush' to remove borders. Defaults to nil.

BODY: Sequence of strings where every pair represents a title and content for
the accordion item.

---
5 Scenarios to Avoid Them: https://www.nngroup.com/videos/avoid-accordions/
---

Example usage:
  (accordion () \"Title 1\" \"Content 1\" \"Title 2\" \"Content 2\")"
  (let ((class (if flush "accordion accordion-flush" "accordion")))
    `(spinneret:with-html
       (:div :class ,class
             :id ,id
             ,@(loop for (title content) on body by #'cddr
                     for counter from 1
                     for collapse-id = (format nil "collapse-~a-~a" id counter)
                     for collapse-class = (concatenate 'string "accordion-collapse collapse" (when (= counter 1) " show"))
                     for btn-class = (concatenate 'string "accordion-button" (when (not (= counter 1)) " collapsed"))
                     collect `(:div :class "accordion-item"
                                    (:h2 :class "accordion-header"
                                         (:button :class ,btn-class
                                                  :type "button"
                                                  :data-bs-toggle "collapse"
                                                  :data-bs-target ,(concatenate 'string "#" collapse-id)
                                                  :aria-expanded ,(if (= counter 1) "true" "false")
                                                  :aria-controls ,collapse-id
                                                  ,title))
                                    (:div :id ,collapse-id
                                          :class ,collapse-class
                                          :data-bs-parent ,(concatenate 'string "#" id)
                                          (:div :class "accordion-body"
                                                ,content))))))))
