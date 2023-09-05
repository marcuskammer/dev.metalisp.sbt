(defpackage cl-sbt/form
  (:use
   :cl)
  (:import-from
   :cl-sbt/btn
   :btn-outline-success
   :btn-primary)
  (:import-from
   :cl-sbt/utility
   :spacing)
  (:export
   :find-l10n
   :ctrl
   :ctrl-col
   :select
   :select-option
   :checkable
   :search-form))

(in-package :cl-sbt/form)

(defun find-l10n (key lang)
  "Finds the localized string for a given key and language.

KEY: The key to look up the localization for.

LANG: The language to get the localized string for."
  (let ((l10n '(("submit" ("en" "Submit" "de" "Absenden" "fr" "Soumettre"))
                ("cancel" ("en" "Cancel" "de" "Abbrechen" "fr" "Annuler"))
                ("search" ("en" "Search" "de" "Suchen" "fr" "Cherchent")))))
    (cadr (member lang (cadr (assoc key l10n :test #'string=)) :test #'string=))))

(defun clean-form-str (str)
  "Cleans a form string for use as a name or identifier.

STR: The string to clean. Removes leading and trailing spaces, replaces spaces
with dashes, and converts to lowercase."
  (string-downcase (substitute #\- #\Space (string-trim '(#\Space) str))))

(defun checkable (type name value)
  "Generates a checkable form control (e.g., radio or checkbox).

TYPE: The type of the control (either 'radio' or 'checkbox').

NAME: The name attribute for the control.

VALUE: The value attribute for the control."
  (let ((name-str (concatenate 'string "group-" (clean-form-str name)))
         (value-str (string-trim '(#\Space) value)))
    (spinneret:with-html
      (:label :class "form-label"
              (:input :type type :name name-str :value (clean-form-str value-str))
              (format nil " ~a" value-str)))))

(defun ctrl-describe (id text)
  "Generates a descriptive text element for a form control.

ID: The unique identifier for the descriptive text.

TEXT: The actual description."
  (spinneret:with-html
    (:div :id id :class "form-text" text)))

(defun ctrl-1 (type name label)
  "Generates a basic Bootstrap form control with a label.

TYPE: Specifies the type of input, such as 'text', 'password', etc.

NAME: The name attribute for the control.

LABEL: The label to display next to the control."
  (let ((name-str (concatenate 'string "group-" (clean-form-str name))))
    (spinneret:with-html
      (:div :class (spacing :property "m" :side "b" :size 3)
            (:label :class "form-label" label
                    (:input :class "form-control" :type type :name name-str))))))

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
                        spinneret:*html-lang*)))))
