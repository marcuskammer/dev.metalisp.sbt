;;;; button.lisp
;;;;
;;;; This file defines a package for generating Bootstrap button components
;;;; using Common Lisp macros.

(defpackage dev.metalisp.sbt/btn
  (:use :cl)
  (:export
   :btn
   :btn-primary
   :btn-secondary
   :btn-success
   :btn-danger
   :btn-warning
   :btn-info
   :btn-light
   :btn-dark
   :btn-link
   :btn-primary-lg
   :btn-secondary-lg
   :btn-success-lg
   :btn-danger-lg
   :btn-warning-lg
   :btn-info-lg
   :btn-light-lg
   :btn-dark-lg
   :btn-link-lg
   :btn-primary-sm
   :btn-secondary-sm
   :btn-success-sm
   :btn-danger-sm
   :btn-warning-sm
   :btn-info-sm
   :btn-light-sm
   :btn-dark-sm
   :btn-link-sm
   :btn-outline-primary
   :btn-outline-secondary
   :btn-outline-success
   :btn-outline-danger
   :btn-outline-warning
   :btn-outline-info
   :btn-outline-light
   :btn-outline-dark
   :btn-outline-link
   :btn-outline-primary-lg
   :btn-outline-secondary-lg
   :btn-outline-success-lg
   :btn-outline-danger-lg
   :btn-outline-warning-lg
   :btn-outline-info-lg
   :btn-outline-light-lg
   :btn-outline-dark-lg
   :btn-outline-link-lg
   :btn-outline-primary-sm
   :btn-outline-secondary-sm
   :btn-outline-success-sm
   :btn-outline-danger-sm
   :btn-outline-warning-sm
   :btn-outline-info-sm
   :btn-outline-light-sm
   :btn-outline-dark-sm
   :btn-outline-link-sm
   :with-btn-group
   :with-btn-group-primary
   :with-btn-group-secondary
   :with-btn-group-success
   :with-btn-group-danger
   :with-btn-group-warning
   :with-btn-group-info
   :with-btn-group-light
   :with-btn-group-dark
   :with-btn-group-link))

(in-package :dev.metalisp.sbt/btn)

(defmacro btn ((&key (id nil) (type "button") (color "primary") (size "")) &body body)
  "This macro generates a Bootstrap button.

ID: (optional) The HTML id attribute for the button.

COLOR: The color of the button (like 'primary', 'secondary', 'success', etc.).

SIZE: (optional) The size of the button ('lg' for large, 'sm' for small).

BODY: The contents of the button.

Example:
  (btn (:type \"danger\" :size \"lg\") \"Delete\")"
  (let ((class-str (concatenate 'string
                                "btn"
                                (format nil " btn-~a" color)
                                (if (string-equal size "") nil (format nil " btn~a" size)))))
    `(spinneret:with-html
       (:button :type ,type
                :class ,class-str
                ,@(when (stringp id) (list :id id))
                ,@body))))

(defmacro define-btn (color &optional (outline nil) (size nil))
  "This macro defines a new macro for creating a Bootstrap button of a specific
   type, size, and outline style.

COLOR: The color of the button (like 'primary', 'secondary', 'success', etc.).

OUTLINE: (optional) Whether the button should be of the outline style.

SIZE: (optional) The size of the button ('lg' for large, 'sm' for small).

The newly defined macro, when called, will generate HTML for a Bootstrap
button of the specified type and size."
  (let* ((size-name (if (null size) "" (format nil "-~a" size)))
         (outline-name (if (null outline) "" "outline-"))
         (color-name (concatenate 'string outline-name color))
         (macro-name (intern (string-upcase (concatenate 'string "BTN-" outline-name color size-name)))))
    `(defmacro ,macro-name ((&key (id nil) (type "button")) &body body)
       `(btn (:id ,id :type ,type :color ,,color-name :size ,,size-name) ,@body))))

(defmacro define-btns (colors)
  "This macro generates a suite of button-creating macros for each provided button type.

NAMES: A list of button type names. Each name should be a string
representing a Bootstrap button type (like 'primary', 'secondary',
'success', etc.).

For each type name in NAMES, this macro defines six new macros: a standard
button, an outline button, a large button, a small button, a large outline
button, and a small outline button.

The newly defined macros, when called, will generate HTML for a Bootstrap
button of the corresponding type, size, and outline style."
  `(progn
     ,@(loop for color in colors
             for color-name = (string-downcase (string color))
             collect `(progn
                        (define-btn ,color-name)
                        (define-btn ,color-name t)
                        (define-btn ,color-name t "lg")
                        (define-btn ,color-name t "sm")
                        (define-btn ,color-name nil "lg")
                        (define-btn ,color-name nil "sm")))))

(define-btns (primary secondary success danger warning info light dark link))

(defmacro with-btn-group ((&key (color "primary")) &rest buttons)
  `(spinneret:with-html
     (:div :class "d-flex justify-content-between"
           ,@(loop for (label url) on buttons by #'cddr
                   collect `(:a :class ,(format nil "btn btn-~a" color)
                                :href ,url
                                ,label)))))

(defmacro define-btn-group (style)
  (let ((macro-name (intern (string-upcase (format nil "WITH-BTN-GROUP-~A" style)))))
    `(defmacro ,macro-name (&rest buttons)
       `(with-btn-group (:color ,,style) ,@buttons))))

(define-btn-group primary)
(define-btn-group secondary)
(define-btn-group success)
(define-btn-group danger)
(define-btn-group warning)
(define-btn-group info)
(define-btn-group light)
(define-btn-group dark)
(define-btn-group link)
