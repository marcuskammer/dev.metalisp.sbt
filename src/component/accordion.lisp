;;;; accordion.lisp
;;;;
;;;; This file defines a package for generating Bootstrap accordion components
;;;; using Common Lisp macros.

(defpackage dev.metalisp.sbt/component/accordion
  (:documentation "A package for generating Bootstrap accordions.")
  (:use :cl)
  (:export
   :accordion))

(in-package :dev.metalisp.sbt/component/accordion)

(defmacro accordion ((&key (id "accordionExample") flush) &body body)
  "This macro generates an accordion-style collapsible list.

ID: Specifies a unique identifier for the accordion. Defaults to 'accordionExample'.

FLUSH: If t, adds class 'accordion-flush' to remove borders. Defaults to nil.

---
5 Scenarios to Avoid Them: https://www.nngroup.com/videos/avoid-accordions/
---

Example usage:
  (accordion () (\"Title 1\" \"Content 1\") (\"Title 2\" \"Content 2\"))
  (accordion (:id \"accordionExample\" :flush t) (\"Title 1\" \"Content 1\") (\"Title 2\" \"Content 2\"))"
  (let ((class (concatenate 'string "accordion" (when flush " accordion-flush"))))
    `(spinneret:with-html
       (:div :class ,class
             :id ,id
             ,@(loop for (title content) in body
                     for counter from 1
                     for collapse-id = (format nil "collapse-~a-~a" id counter)
                     collect `(:div :class "accordion-item"
                                    (:h2 :class "accordion-header"
                                         (:button :class "accordion-button"
                                                  :type "button"
                                                  :data-bs-toggle "collapse"
                                                  :data-bs-target ,(concatenate 'string "#" collapse-id)
                                                  :aria-expanded ,(if (= counter 1) "true" "false")
                                                  :aria-controls ,collapse-id
                                                  ,title))
                                    (:div :id ,collapse-id
                                          :class "accordion-collapse collapse"
                                          :data-bs-parent ,(concatenate 'string "#" id)
                                          (:div :class "accordion-body"
                                                ,content))))))))
