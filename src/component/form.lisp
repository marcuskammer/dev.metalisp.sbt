(defpackage cl-sbt/form
  (:use
   :cl)
  (:import-from
   :cl-sbt/btn
   :btn-outline-success
   :btn-primary)
  (:export
   :find-l10n
   :ctrl
   :ctrl-col
   :select
   :select-option
   :choice
   :search-form))

(in-package :cl-sbt/form)

(defun find-l10n (key lang)
  (let ((l10n '((submit ("en" "Submit" "de" "Absenden" "fr" "Soumettre"))
                (cancel ("en" "Cancel" "de" "Abbrechen" "fr" "Annuler")))))
    (cadr (member lang (cadr (assoc key l10n)) :test #'string=))))

(defmacro ctrl (&rest rest)
  "This macro generates Bootstrap form controls.

ID: A unique identifier for the input control.

LABEL: The label text displayed next to the form control.

TYPE: Specifies the type of input. For example, 'text', 'password', etc.

PLACEHOLDER: A hint to the user of what can be entered in the control.

TEXT: Descriptive text about the input field, often used for instructions or
details.

DESCRIBEBY: Refers to the id of the element that describes the input field.

Example:
  (ctrl (:id \"inputID\"
         :label \"Label\"
         :type \"text\"
         :placeholder \"Placeholder\"
         :describeby \"hintID\"
         :text \"Hint text\"))"
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key id label type placeholder text describeby) item
                       `(:div :class "mb-3"
                              (:label :for ,id
                                      :class "form-label"
                                      ,label)
                              (:input :type ,type
                                      :class "form-control"
                                      :id ,id
                                      :placeholder ,placeholder
                                      ,@(when (stringp describeby)
                                          (list :aria-describeby describeby)))
                              ,(if (stringp text)
                                   `(:div :id ,describeby
                                          :class "form-text"
                                          ,text)
                                   nil))))))

(defmacro ctrl-col (&rest rest)
  "This macro generates Bootstrap form controls arranged in a column.

ID: A unique identifier for the input control.

LABEL: The label text displayed next to the form control.

TYPE: Specifies the type of input, e.g., 'text', 'password', etc.

PLACEHOLDER: A hint to the user of what can be entered in the control.

Example:
  (ctrl-col (:id \"inputID\" :label \"Label\" :type \"text\" :placeholder \"Placeholder\"))"
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key id label type placeholder) item
                       `(:div :class "row g-3 align-items-center"
                              (:div :class "col-auto"
                                    (:label :for ,id
                                            :class "col-form-label"
                                            ,label))
                              (:div :class "col-auto"
                                    (:input :class "form-control"
                                            :type ,type
                                            :id ,id
                                            :placeholder ,placeholder)))))))

(defmacro select ((&key size) &rest rest)
  "This macro generates a Bootstrap select dropdown menu.

SIZE: Specifies the size of the select menu. It can be a number specifying the
visible number of options or a string indicating the size, like 'sm' for small
or 'lg' for large. The special string 'multiple' can also be used to allow for
multiple selections.

BODY: The contents of the select menu, typically options.

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
         (:option :selected t "Open this selected menu")
         ,@(loop for item in rest
                 collect (destructuring-bind (&key content value) item
                           `(:option :value ,value ,content)))))))

(defun choice (text name type)
  "This macro generates a list item for an answer option in a question.

TEXT: The display text of the answer option.

NAME: Specifies the name attribute for the input element. This should align
with the name specified in the containing question.

TYPE: Specifies the type of input element, such as \"radio\" for radio buttons.

Example usage:
  (choice \"18-24\" \"age\" \"radio\")"
  (spinneret:with-html
    (:li (:label :class "form-label"
                 (:input :type type :name name :value text)
                 (format nil " ~a" text)))))

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
           (btn-outline-success (:type "submit") "Search"))))
