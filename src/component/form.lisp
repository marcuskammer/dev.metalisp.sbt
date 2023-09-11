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
   :find-l10n)
  (:import-from
   :cl-sbt/btn
   :btn-outline-success
   :btn-primary)
  (:import-from
   :cl-sbt/utility
   :spacing)
  (:export
   :l10n
   :select
   :checkable
   :ctrl
   :search-form))

(in-package :cl-sbt/form)

(defvar l10n '(("submit" ("en" "Submit" "de" "Absenden" "fr" "Soumettre"))
               ("cancel" ("en" "Cancel" "de" "Abbrechen" "fr" "Annuler"))
               ("upload" ("en" "Upload" "de" "Hochladen" "fr" "Télécharger"))
               ("search" ("en" "Search" "de" "Suchen" "fr" "Cherchent"))
               ("select-option" ("en" "Open this selected menu"
                                 "de" "Das ausgewählte Menü öffnen"
                                 "fr" "Ouvrir le menu sélectionné"))))

(defun remove-special-chars (str)
  "Removes all special characters from the string STR except numbers and
   alphabets.

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
with dashes, and converts to lowercase."
  (string-downcase (substitute #\- #\Space (string-trim '(#\Space) str))))

(defun build-name-str (name)
  "Builds a standardized string by adding a 'group-' prefix and applying
   cleaning functions.

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
  "Builds an ID string by concatenating a standardized name string and a
   sanitized value property string.

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
      (:div :class "form-check"
            (:label :class class-str
                    :for id-str
                    (:input :type type
                            :name name-str
                            :value value-prop-str
                            :id id-str
                            :class "form-check-input")
                    (format nil " ~a" value-str))))))

(defun ctrl (type name label)
  "Generates a basic Bootstrap form control with a label.

TYPE: Specifies the type of input, such as 'text', 'password', etc.

NAME: The name attribute for the control.

LABEL: The label to display next to the control."
  (let* ((name-str (build-name-str name))
         (class-str (build-class-str "form-label" name))
         (id-str (build-id-str name label)))
    (spinneret:with-html
      (:div :class (spacing :property "m" :side "b" :size 3)
            (:label :class class-str
                    :for id-str
                    label)
            (:input :class "form-control"
                    :id id-str
                    :type type
                    :name name-str)))))

(defmacro select ((&key size) &rest rest)
  "This macro generates a Bootstrap select dropdown menu.

SIZE: Specifies the size of the select menu. It can be a number specifying the
visible number of options or a string indicating the size, like 'sm' for small
or 'lg' for large. The special string 'multiple' can also be used to allow for
multiple selections.

REST: The contents of the select menu, typically options.

Example:
  (select (:size \"sm\")
          (:content \"Option 1\" :value \"opt1\"))"
  (let ((class-attr (cond ((and (stringp size)
                                (string= size "multiple")) "form-select")
                          ((stringp size)
                           (format nil "form-select form-select-~a" size))
                          (t "form-select"))))
    `(spinneret:with-html
         (:select :class ,class-attr
                  ,@(when (numberp size) `(:size ,size))
                  ,@(when (and (stringp size) (string= size "multiple")) (list :multiple t))
                  :aria-label "Default select example"
                  (:option :selected t
                           (find-l10n "option-selected"
                                      spinneret:*html-lang*
                                      l10n))
                  ,@(loop for item in rest
                          collect (destructuring-bind (&key content value) item
                                    `(:option :value ,value ,content)))))))

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
                   :aria-label "Search")
           (btn-outline-success (:type "submit")
             (find-l10n "search"
                        spinneret:*html-lang*
                        l10n)))))
