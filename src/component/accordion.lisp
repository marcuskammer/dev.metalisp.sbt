;;;; accordion.lisp
;;;;
;;;; This file defines a package for generating Bootstrap accordion components
;;;; using Common Lisp macros. The package provides four main macros:
;;;; - header: Generates a Bootstrap header for an accordion item.
;;;; - collapse: Generates a Bootstrap collapse for an accordion item.
;;;; - item: Generates a Bootstrap accordion item.
;;;; - accordion: Generates an accordion-style collapsible list with Bootstrap.

(defpackage cl-sbt/accordion
  (:documentation "A package for generating Bootstrap accordions.")
  (:use :cl)
  (:export
   :header
   :collapse
   :item
   :accordion))

(in-package :cl-sbt/accordion)

(defun header (target name show)
  "This macro generates a Bootstrap header for an accordion item.

TARGET: The id of the collapse this header controls.

NAME: The text that will be displayed on this header.

SHOW: A boolean indicating whether the collapse controlled by this header
should be shown when the accordion first loads. If true, the 'aria-expanded'
attribute will be 'true'.

Example:
  (header \"collapseOne\" \"Heading\" t)"
  (let ((target (concatenate 'string "#" target))
        (show (if (null show) "false" "true")))
    (spinneret:with-html
      (:h2 :class "accordion-header"
           (:button :class "accordion-button"
                    :type "button"
                    :data-bs-toggle "collapse"
                    :data-bs-target target
                    :aria-expanded show
                    :aria-controls target
                    name)))))

(defmacro collapse (parent id show &body body)
  "This macro generates a Bootstrap collapse for an accordion item.

PARENT: The id of the parent element that contains the collapse.

ID: The unique id for this collapse.

SHOW: A boolean indicating whether the collapse should be shown when the
accordion first loads. If true, 'show' will be added to the classes of the
collapse.

BODY: The contents of the collapse.

Example:
  (collapse \"accordionExample\" \"collapseOne\" t \"Some content\")"
  (let ((class (concatenate 'string "accordion-collapse collapse" (if (null show) nil " show")))
        (parent (concatenate 'string "#" parent)))
    `(spinneret:with-html
       (:div :id ,id
             :class ,class
             :data-bs-parent ,parent
             (:div :class "accordion-body"
                   ,@body)))))

(defmacro item (&body body)
  "This macro generates a Bootstrap accordion item.

BODY: The contents of the accordion item.

Example:
  (item (header \"collapseOne\" \"Heading\" t) (collapse \"accordionExample\" \"collapseOne\" t \"Some content\"))"
  `(spinneret:with-html
     (:div :class "accordion-item"
           ,@body)))

(defmacro accordion (id &rest rest)
  "This macro generates an accordion-style collapsible list with Bootstrap.

ID: Specifies a unique identifier for the accordion. Defaults to 'accordionExample'.

REST: Specifies a list of accordion items. Each item is a plist with the following keys:
- :target: Specifies a unique identifier for the accordion item.
- :name: Specifies the name of the accordion item.
- :show: Specifies whether the accordion item should be displayed by default.
- :content: Specifies the content of the accordion item.

Example:
  (accordion \"accordionExample\"
             (:target \"collapseOne\" :name \"Accordion Item #1\" :show t :content \"This is the first item's accordion body.\")
             (:target \"collapseTwo\" :name \"Accordion Item #2\" :content \"This is the second item's accordion body.\")
             (:target \"collapseThree\" :name \"Accordion Item #3\" :content \"This is the second item's accordion body.\"))"
  `(spinneret:with-html
     (:div :class "accordion"
           :id ,id
           ,@(loop for item in rest
                   collect (destructuring-bind (&key target name show content) item
                             `(item (header ,target ,name ,show)
                                (collapse ,id ,target ,show ,content)))))))
