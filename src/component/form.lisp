;;;; -*- mode: lisp; coding: utf-8-unix; tab-width: 4; fill-column: 100; indent-tabs-mode: nil; -*-
;;;; form.lisp
;;;;
;;;; This file is part of the CL-SBT project and defines utility functions,
;;;; macros, and HTML templates for generating Bootstrap-formatted forms.
;;;; It provides mechanisms to build form controls, manage localization,
;;;; and perform string manipulations.

(defpackage cl-sbt/form
  (:use
   :cl)
  (:import-from
   :cl-sbt
   :*l10n*
   :find-l10n)
  (:import-from
   :cl-sbt/btn
   :btn-outline-success
   :btn-primary)
  (:import-from
   :cl-sbt/utility
   :spacing)
  (:export
   :combo
   :combo-lg
   :combo-sm
   :combo-multiple
   :checkable
   :checkable-radio
   :checkable-checkbox
   :ctrl
   :search-form))

(in-package :cl-sbt/form)

(defun remove-special-chars (str)
  "Removes all special characters from the string STR except numbers and alphabets.

STR: The input string from which special characters need to be removed.

Example:
  (remove-special-chars \"a1b!@#$%^&*()c2\") will return \"a1bc2\"

Returns:
  A new string with special characters removed."
  (remove-if-not #'(lambda (char)
                     (or (alpha-char-p char) (digit-char-p char)))
                 str))

(defun clean-form-str (str)
  "Cleans a form string for use as a name or identifier.

STR: The string to clean. Removes leading and trailing spaces, replaces spaces
with dashes, and converts to lowercase.

Returns:
  A new string which can be used as HTML class."
  (string-downcase (substitute #\- #\Space (string-trim '(#\Space) str))))

(defun build-name-str (name)
  "Builds a standardized string by adding a 'group-' prefix and applying cleaning functions.

NAME: The initial name string.

Returns:
  A new standardized string."
  (concatenate 'string "group-" (clean-form-str name)))

(defun build-value-str (value)
  "Trims leading and trailing spaces from the given value string.

VALUE: The string to be cleaned.

Returns:
  A new string without leading and trailing spaces."
  (string-trim '(#\Space) value))

(defun build-value-prop-str (value)
  "Builds a value property string by applying various cleaning functions.

VALUE: The initial value string.

Returns:
  A new value property string."
  (clean-form-str (build-value-str value)))

(defun build-class-str (class name)
  "Builds a class string by concatenating 'form-check-label' and a standardized name string.

CLASS: Corresponding class property.

NAME: The initial name string.

Returns:
  A new class string."
  (concatenate 'string class " " (build-name-str name)))

(defun build-id-str (name value)
  "Builds an ID string by concatenating a standardized name string and a sanitized value property string.

NAME: The initial name string.

VALUE: The initial value string.

Returns:
  A new ID string."
  (concatenate 'string
               (build-name-str name)
               "-"
               (remove-special-chars (build-value-prop-str value))))

(defun checkable (type name value)
  "Generates a checkable form control (e.g., radio or checkbox).

TYPE: The type of the control (either 'radio' or 'checkbox').

NAME: The name attribute for the control.

VALUE: The value attribute for the control."
  (let* ((name-str (build-name-str name))
         (value-str (build-value-str value))
         (value-prop-str (build-value-prop-str value))
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
  (let ((func-name (intern (string-upcase (concatenate 'string "checkable-" type)))))
    `(defun ,func-name (name value)
       (checkable ,type name value))))

(defmacro define-checkables (types)
  "Generates multiple checkable functions based on the provided list of types.

TYPES: A list of strings that specifies the types for which to generate
checkable macros."
  `(progn
     ,@(loop for type in types collect `(define-checkable ,type))))

(define-checkables ("radio" "checkbox"))

(defun ctrl (type name label)
  "Generates a basic Bootstrap form control with a label.

TYPE: Specifies the type of input, such as: 'button', 'checkbox', 'color', 'date', 'datetime-local',
'email', 'file', 'hidden', 'image', 'month', 'number', 'password', 'radio', 'range', 'reset',
'search', 'submit', 'tel' 'text', 'time', 'url', 'week'

NAME: The name attribute for the control.

LABEL: The label to display next to the control."
  (let* ((name-str (build-name-str name))
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
  (let ((class-attr (cond ((stringp size)
                           (format nil "form-select form-select-~a" size))
                          (t "form-select"))))
    `(spinneret:with-html
       (:comment "FORM/SELECT")
       (:select :class ,class-attr
         ,@(when (numberp multiple) (list :size multiple :multiple t))
         (:option :selected t
                  (find-l10n "option-selected" spinneret:*html-lang* *l10n*))
         ,@(loop for item in body
                 collect
                 (let ((value-prop-str (build-value-prop-str item)))
                   `(:option :value ,value-prop-str ,item)))))))

(defmacro combo-multiple (rows &body body)
  `(combo (:multiple ,rows) ,@body))

(defmacro define-combo (size)
  "Defines a new select macro tailored for a given size.

SIZE: A string that specifies the size ('lg' for large, 'sm' for small)."
  (let ((macro-name (intern (string-upcase (concatenate 'string "COMBO-" size)))))
    `(defmacro ,macro-name (&body body)
       `(combo (:size ,,size) ,@body))))

(defmacro define-combos (sizes)
  "Generates multiple select macros based on the provided list of sizes.

SIZES: A list of strings that specifies the sizes for which to generate select
macros."
  `(progn
     ,@(loop for size in sizes
             collect `(define-combo ,size))))

(define-combos ("lg" "sm"))

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
