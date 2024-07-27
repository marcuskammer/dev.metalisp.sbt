;;;; -*- mode: lisp; coding: utf-8; fill-column: 84; indent-tabs-mode: nil; -*-
;;;; form.lisp

;;;; This file is part of the dev.metalisp.sbt project and defines utility
;;;; functions, macros, and HTML templates for generating Bootstrap-formatted
;;;; forms.

(defpackage dev.metalisp.sbt/form
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
   :build-str-class
   :build-str-id)
  (:import-from
   :dev.metalisp.sbt/btn
   :btn-outline-success
   :btn-primary)
  (:import-from
   :dev.metalisp.sbt/utility
   :spacing)
  (:export
   :root
   :multi-form
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
   :search-form
   :with-form))

(in-package :dev.metalisp.sbt/form)

;;; root

(defmacro root (action method &body body)
  "Generates HTML form element.

LEGEND: Add text for <legend>. A short description for forms.

ATTR: A list of form attributes like :action, :name and :method. If not provided,
defaults to an empty action, \"html-form\" as name and \"post\" as method.

BODY: The body of the form, which generally includes form elements such as input
fields, checkboxes, radio buttons, etc."
  `(spinneret:with-html
     (:form :action ,action :method ,method ,@body)))

;;; checkable

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
         (class-str (build-str-class "form-check-label" name))
         (id-str (build-str-id name value)))
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

;;; ctrl

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
         (class-str (build-str-class "form-label" name))
         (id-str (build-str-id name label)))
    (spinneret:with-html
      (:comment (format nil "FORM/CTRL ~A ~A" type name))
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

;;; combo

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

;;; other

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

;;; TODO input groups

;;; form DSL

;;; form

(deftype composite-list ()
  "Represents a composite list that can either be a valid `choices` or `question`.

A composite list is expected to satisfy either the `choicesp` or `questionp` predicate."
  '(or (satisfies choicesp)
       (satisfies questionp)))

(defun choicep (lst)
  "Checks if the given list LST is a valid choice.

A valid choice list starts with a keyword, followed by strings.

Returns T if it's a valid choice list, otherwise NIL."
  (and (keywordp (first lst))
       (every #'stringp (rest lst))))
(deftype choice ()
  "Represents a valid choice list.

A choice list is expected to satisfy the `choicep` predicate."
  '(and list (satisfies choicep)))

(defun choicesp (lst)
  "Checks if the given list LST contains only keyword or string elements.

Returns T if all elements are either keywords or strings, otherwise NIL."
  (loop for elem in lst
        always (and (keywordp (first lst))
                    (or (keywordp elem)
                        (stringp elem)))))
(deftype choices ()
  "Represents a valid choices list.

A choices list is expected to satisfy the `choicesp` predicate."
  '(and list (satisfies choicesp)))

(defun questionp (lst)
  "Checks if the given list LST is a valid question.

A valid question is a list of alternating keywords and either strings or lists
satisfying `choicesp`.

Returns T if it's a valid question, otherwise NIL."
  (if (= (length lst) 8)
      (loop for i from 0 below (length lst)
            for elem = (nth i lst)
            always (if (evenp i)
                       (keywordp elem)
                       (or (stringp elem)
                           (choicesp elem))))
      nil))
(deftype question ()
  "Represents a valid question list.

A question list is expected to satisfy the `questionp` predicate."
  '(and list (satisfies questionp)))

(defun extract-question-components (question)
  "Extracts components of a question stored as a plist.

QUESTION: A plist representing a question.

Returns multiple values:
  - The question text (ASK)
  - The group name (GROUP)
  - The style (STYLE)
  - The choices (CHOICES)"
  (let ((splitted-list (split-list-by-keyword question)))
    (apply #'values (mapcar (lambda (x) (nth 1 x)) splitted-list))))

(declaim (ftype (function (composite-list) list) split-list-by-keyword))
(defun split-list-by-keyword (lst)
  "Splits a list (LST) into a list of smaller lists, each starting with a keyword.

LST: A list that includes keywords followed by their associated values. The
list can be a standard property list or a key-grouped list.

This function treats all elements after each keyword and before the next
keyword as its values, and each new keyword signifies the start of a new
sublist.

Example 1 (Property List):
  Given the plist '(:a 1 :b 2 :c 3),
  it will return '((:a 1) (:b 2) (:c 3)).

Example 2 (Key-Grouped List):
  Given the list '(:a 1 2 3 :b 4 5),
  it will return '((:a 1 2 3) (:b 4 5)).

Returns:
  A list of sublists, each starting with a keyword."
  ;; Initialize result and current-list
  (let ((result '())
        (current-list '()))
    ;; Loop through each item in plist
    (loop for item in lst
          do (if (keywordp item)  ; Check if item is a keyword
                 ;; Start of new property list detected
                 (progn
                   ;; Add current list to result if it is not empty
                   (when current-list
                     (push (nreverse current-list) result))
                   ;; Reset current-list with the new keyword
                   (setq current-list (list item)))
                 ;; Add item to the current property list
                 (push item current-list)))
    ;; Add remaining current-list to result
    (when current-list
      (push (nreverse current-list) result))
    ;; Return the reversed result list
    (nreverse result)))

(declaim (ftype (function (choice) (values string list)) resolve-input-and-choice))
(defun resolve-input-and-choice (choice)
  "Separate the input-type keyword from the remaining CHOICE in a list.

If the first element of CHOICE is a keyword, it is taken to be the input-type
keyword, and the rest of the list is taken to be the actual values.

CHOICE: The choice list, including an input type keyword.

Returns two values:
  1. The input-type string if a keyword.
  2. The remaining values in the list, excluding the input type keyword."
  (let ((input-type-keyword (first choice)))
    (if (keywordp input-type-keyword)
        (values (resolve-input-type (string-downcase input-type-keyword)) (rest choice))
        (error "A choice always starts with a input-type keyword"))))

(defun apply-input-form (type group item)
  "Apply the chosen input form function to generate HTML for a single form element.

TYPE: A string specifying the HTML input type like 'radio', 'checkbox', 'text',
etc.

GROUP: A string specifying the name attribute for the input elements.

ITEM: The particular choice item that this form element represents.

Returns:
  The HTML form element generated for the ITEM."
  (funcall (choose-input-form type) type group item))

(defun choose-input-form (type)
  "Choose the appropriate function to generate the HTML form input based on TYPE.

TYPE: A string specifying the HTML input type like 'radio', 'checkbox', 'text',
etc.

Returns:
  A function that can be used to generate the HTML form input. Or throws an error
  if an unknown type is passed."
  (typecase type
    (checkable-element #'checkable)
    (ctrl-element #'ctrl)
    (otherwise (error "Unknown type ~A" type))))

(defun resolve-input-type (type)
  "Resolve the given input TYPE keyword to the corresponding HTML input type.

The function maps specific keywords to HTML input types. For example, it maps
\"single\" to \"radio\" and \"multiple\" to \"checkbox\". If the input TYPE
does not match these special cases, it is returned as-is.

TYPE: The input type keyword to resolve.

Returns:
  The corresponding HTML input type string."
  (cond ((string= type "single") "radio")
        ((string= type "multiple") "checkbox")
        (t type)))

(defmacro multi-form (&body body)
  "This macro generates an HTML form composed of multiple parts.

BODY: A series of parts. Each part should contain the keys :ask, :group, :style and
:choices. The first element of :choices should be a keyword specifying the type of
input elements (e.g. :radio), followed by a list of answer options.

Example 1:
  (multi-form
        (:ask \"How old are you?\"
         :group \"age\"
         :style nil
         :choices (:radio \"18-24\" \"25-34\" \"35-44\")))"
  `(spinneret:with-html
     ,@(loop for q in body
             for (ask group style choices) = (multiple-value-list (extract-question-components q))
             collect `(:fieldset (:legend ,ask)
                                 (:ol ,@(when style (list :style style))
                                      ,@(loop for choice in (split-list-by-keyword choices)
                                              for (type values) = (multiple-value-list (resolve-input-and-choice choice))
                                              collect `(progn
                                                         ,(if (string= type "combo")
                                                              `(:li (:select ,@(loop for value in values
                                                                                     collect `(:option ,value))))
                                                              `(progn ,@(loop for value in values
                                                                              collect `(:li (apply-input-form ,type ,group ,value))))))))))))

(defmacro with-form ((&optional action lang intro) &body body)
  "Create a standardized HTML form wrapped in a <main> tag with a pre-defined
class and structure, using the Spinneret library.

ACTION: Form action.

LANG: Language for l10n."
  `(spinneret:with-html
     (:div :class (spacing :property "m" :side "y" :size 5)
           (when ,intro (:p ,intro))
           (:form :action (unless ,action (hunchentoot:request-uri*))
                  :method "post"
                  :class (spacing :property "m" :side "y" :size 1)
                  ,@body
                  (btn-primary (:type "submit")
                    (find-l10n "submit" ,lang *l10n*))))))
