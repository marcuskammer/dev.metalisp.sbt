;;;; -*- mode: lisp; coding: utf-8; fill-column: 84; indent-tabs-mode: nil; -*-
;;;; form.lisp

;;;; This file is part of the dev.metalisp.sbt project and defines utility
;;;; functions, macros, and HTML templates for generating Bootstrap-formatted
;;;; forms.

(defpackage dev.metalisp.sbt/component/form
  (:use
   :cl)
  (:import-from
   :dev.metalisp.sbt
   :*l10n*
   :find-l10n
   :remove-special-chars
   :clean-form-str
   :build-str-name
   :build-str-value
   :build-str-value-prop
   :build-class-str
   :build-id-str)
  (:import-from
   :dev.metalisp.sbt/component/btn
   :btn-outline-success
   :btn-primary)
  (:import-from
   :dev.metalisp.sbt/utility
   :spacing)
  (:export
   :checkable
   :checkable-radio
   :checkable-checkbox
   :checkable-element
   :combo
   :combo-lg
   :combo-sm
   :combo-multiple
   :ctrl
   :ctrl-button
   :ctrl-checkbox
   :ctrl-color
   :ctrl-date
   :ctrl-datetime-local
   :ctrl-email
   :ctrl-file
   :ctrl-hidden
   :ctrl-image
   :ctrl-month
   :ctrl-number
   :ctrl-password
   :ctrl-radio
   :ctrl-range
   :ctrl-reset
   :ctrl-search
   :ctrl-submit
   :ctrl-tel
   :ctrl-text
   :ctrl-time
   :ctrl-url
   :ctrl-week
   :ctrl-element
   :search-form))

(in-package :dev.metalisp.sbt/component/form)

(defvar checkable-elements
  '("radio" "checkbox")
  "List of checkable specific input elements ")

(defun checkable-element-p (checkable-element)
  (find checkable-element checkable-elements :test #'string=))

(deftype checkable-element ()
  '(and string (satisfies checkable-element-p)))

(defun checkable (type name value)
  "Generates a checkable form control.

TYPE: A string representing checkable-element.

NAME: The name attribute for the control.

VALUE: The value attribute for the control."
  (check-type type checkable-element)
  (let* ((name-str (build-str-name name))
         (value-str (build-str-value value))
         (value-prop-str (build-str-value-prop value))
         (class-str (build-class-str "form-check-label" name))
         (id-str (build-id-str name value)))
    (spinneret:with-html
      (:comment "FORM/CHECKABLE")
      (:div :class "form-check"
            (:label :class class-str
                    :for id-str
                    (:input :type type
                            :name name-str
                            :value value-prop-str
                            :id id-str
                            :class "form-check-input")
                    (format nil " ~a" value-str))))))

(defmacro define-checkable (type)
  "Generates a checkable function based on the provided type.

TYPE: A string representing checkable-element."
  (let ((func-name (intern (string-upcase (concatenate 'string "checkable-" type)))))
    `(defun ,func-name (name value)
       (checkable ,type name value))))

(defmacro define-checkables ()
  "Generates multiple checkable functions based on the provided list of types.

TYPES: A list of strings that specifies the types for which to generate
checkable macros."
  `(progn
     ,@(loop for type in '("radio" "checkbox")
             collect `(define-checkable ,type))))

(define-checkables)

(defvar ctrl-elements
  '("button" "checkbox" "color" "date" "datetime-local" "email" "file" "hidden"
    "image" "month" "number" "password" "radio" "range" "reset" "search" "submit"
    "tel" "text" "time" "url" "week")
  "List of input elements.
See: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input")

(defun ctrl-element-p (ctrl-element)
  "Test for HTML input type.

CTRL-ELEMENT: String

Returns:
  t or nil"
  (find ctrl-element ctrl-elements :test #'string=))

(deftype ctrl-element ()
  '(and string (satisfies ctrl-element-p)))

(defun ctrl (type name label)
  "Generates a basic Bootstrap form control with a label.

TYPE: A string representing ctrl-element.

NAME: The name attribute for the control.

LABEL: The label to display next to the control."
  (check-type type ctrl-element)
  (let* ((name-str (build-str-name name))
         (class-str (build-class-str "form-label" name))
         (id-str (build-id-str name label)))
    (spinneret:with-html
      (:comment "FORM/CTRL")
      (:div :class (spacing :property "m" :side "b" :size 3)
            (:label :class class-str
                    :for id-str
                    label)
            (:input :class "form-control"
                    :id id-str
                    :type type
                    :name name-str)))))

(defmacro define-ctrl (type)
  "Generates a checkable function based on the provided type.

TYPE: A string representing checkable-element."
  (let ((func-name (intern (string-upcase (concatenate 'string "ctrl-" type)))))
    `(defun ,func-name (name value)
       (ctrl ,type name value))))

(defmacro define-ctrls ()
  "Generates multiple checkable functions based on the provided list of types.

TYPES: A list of strings that specifies the types for which to generate
checkable macros."
  `(progn
     ,@(loop for type in '("button" "checkbox" "color" "date" "datetime-local"
                           "email" "file" "hidden" "image" "month" "number"
                           "password" "radio" "range" "reset" "search" "submit"
                           "tel" "text" "time" "url" "week")
             collect `(define-ctrl ,type))))

(define-ctrls)

(defmacro combo ((&key size multiple) &body body)
  "This macro generates a Bootstrap select dropdown menu.

SIZE: Specifies the size of the select menu. It can be a string indicating the
size, like 'sm' for small or 'lg' for large.

MULTIPLE: If specified as a number, allows multiple selections. And makes the
select a scrolling list box. This attribute represents the number of rows in
the list that should be visible at one time.

REST: The contents of the select menu, typically options.

Example 1:
  (combo (:size \"sm\") \"Red\" \"Green\" \"Blue\"

Example 2:
  (combo (:multiple 3) \"Red\" \"Green\" \"Blue\""
  (let ((class-attr (if (stringp size)
                        (format nil "form-select form-select-~a" size)
                        "form-select")))
    `(spinneret:with-html
       (:comment "FORM/SELECT")
       (:select :class ,class-attr
         ,@(when (numberp multiple) (list :size multiple :multiple t))
         (:option :selected t
                  (find-l10n "option-selected" spinneret:*html-lang* *l10n*))
         ,@(loop for item in body
                 collect
                 (let ((value-prop-str (build-str-value-prop item)))
                   `(:option :value ,value-prop-str ,item)))))))

(defmacro combo-multiple (rows &body body)
  `(combo (:multiple ,rows) ,@body))

(defmacro define-combo (size)
  "Defines a new select macro tailored for a given size.

SIZE: A string that specifies the size ('lg' for large, 'sm' for small)."
  (let ((macro-name (intern (string-upcase (concatenate 'string "COMBO-" size)))))
    `(defmacro ,macro-name (&body body)
       `(combo (:size ,,size) ,@body))))

(defmacro define-combos ()
  "Generates multiple select macros based on the provided list of sizes.

SIZES: A list of strings that specifies the sizes for which to generate select
macros."
  `(progn
     ,@(loop for size in '("lg" "sm")
             collect `(define-combo ,size))))

(define-combos)

(defun search-form ()
  "This function generates a general-purpose search form.

This form uses the HTML5 search input type and contains a search button. It
also utilizes Bootstrap classes for styling. The form uses a `d-flex` class to
display its elements in a flexible layout, allowing the input and button to be
horizontally aligned.

Example usage:
  (search-form)"
  (spinneret:with-html
    (:form :class "d-flex"
           :role "search"
           (:input :class "form-control me-2"
                   :type "search"
                   :placeholder "Search"
                   :title "Search")
           (btn-outline-success (:type "submit")
             (find-l10n "search"
                        spinneret:*html-lang*
                        *l10n*)))))
