(defpackage cl-sbt/form
  (:use
   :cl)
  (:import-from
   :cl-sbt/btn
   :btn-outline-success
   :btn-primary)
  (:export
   :ctrl
   :ctrl-col
   :select
   :select-option
   :search-form
   :question
   :questionnaire))

(in-package :cl-sbt/form)

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
  (ctrl (:id \"inputID\" :label \"Label\" :type \"text\" :placeholder \"Placeholder\" :describeby \"hintID\" :text \"Hint text\"))"
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

(defmacro select-option (&rest rest)
  "This macro generates option elements for a select dropdown.

CONTENT: The text content displayed for the option.

VALUE: The value of the option that gets sent when the form is submitted.

Example:
  (select-option (:content \"Option 1\" :value \"opt1\"))"
  `(spinneret:with-html
     ,@(loop for item in rest
             collect (destructuring-bind (&key content value) item
                       `(:option :value ,value ,content)))))

(defmacro select ((&key size) &body body)
  "This macro generates a Bootstrap select dropdown menu.

SIZE: Specifies the size of the select menu. It can be a number specifying the
visible number of options or a string indicating the size, like 'sm' for small
or 'lg' for large. The special string 'multiple' can also be used to allow for
multiple selections.

BODY: The contents of the select menu, typically options.

Example:
  (select (:size \"sm\")
          (:content \"Option 1\" :value \"opt1\"))"
  (let ((class-attr (cond ((null size) "form-select")
                          ((numberp size) "form-select")
                          ((and (stringp size)
                                (string= size "multiple")) "form-select")
                          ((stringp size)
                           (format nil "form-select form-select-~a" size))
                          (t (error "Invalid size specification: ~a" size)))))
    `(spinneret:with-html
       (:select :class ,class-attr
         ,@(when (numberp size) `(:size ,size))
         ,@(when (and (stringp size) (string= size "multiple")) (list :multiple t))
         :aria-label "Default select example"
         (:option :selected t "Open this selected menu")
         (select-option ,@body)))))

(defun search-form ()
  "Generates a general used search form"
  (spinneret:with-html
    (:form :class "d-flex"
           :role "search"
           (:input :class "form-control me-2"
                   :type "search"
                   :placeholder "Search"
                   :aria-label "Search")
           (btn-outline-success (:type "submit") "Search"))))

(defmacro answer (text name type)
  "This macro generates a list item for an answer option in a question.

TEXT: The display text of the answer option.

NAME: Specifies the name attribute for the input element. This should align
with the name specified in the containing question.

TYPE: Specifies the type of input element, such as \"radio\" for radio buttons.

Example usage:
  (answer \"18-24\" \"age\" \"radio\")"
  `(spinneret:with-html
     (:li (:label :class "form-label"
                  (:input :type ,type :name ,name)
                  ,text))))

(defmacro question (question (&key (group "group") (type "radio")) &rest rest)
  "This macro generates a fieldset for a question with multiple answers.

QUESTION: The text of the question to be displayed in the legend.

NAME: Specifies the name attribute for the input elements. Defaults to an empty
string.

TYPE: Specifies the type of input elements. Commonly used value is \"radio\".
Defaults to an empty string.

REST: A list of strings representing the different answers available for
selection.

Example:
  (question \"How old are you?\"
            (:group \"age\" :type \"radio\") \"18-24\" \"25-34\" \"35-44\")"
  `(spinneret:with-html
     (:fieldset (:legend ,question)
                (:ol ,@(loop for text in rest
                             collect `(answer ,text ,group ,type))))))

(defmacro questionnaire (action &body body)
  "This macro generates an HTML form composed of multiple questions, each
   rendered using the `question` macro.

ACTION: Specifies the URL where the form will be submitted. This should be a
string representing the URL path.

BODY: A series of calls to the `question` macro to define each question.

Example:
  (questionnaire \"/submit\"
                 (question \"How old are you?\" (:name \"age\" :type \"radio\") \"18-24\" \"25-34\" \"35-44\")
                 (question \"What's your favorite color?\" (:name \"color\" :type \"radio\") \"Red\" \"Blue\" \"Green\"))"
  `(spinneret:with-html
     (:form :action ,action
            :method "post"
            ,@body
            (btn-primary (:type "submit") "Submit"))))

(defmacro questionnaire-1 (action &rest questions)
  "This macro generates an HTML form composed of multiple questions, each
   rendered using the `question` macro.

ACTION: Specifies the URL where the form will be submitted. This should be a
string representing the URL path.

QUESTIONS: A series of plists, each representing a question. Each plist should
contain the keys :question, :name, :type, and :answers.

Example:
  (questionnaire \"/submit\" (:ask \"How old are you?\" :group \"age\" :type \"radio\" :answers (\"18-24\" \"25-34\" \"35-44\")))

This will create a form with a question and radio button options, and a Submit button."
  `(spinneret:with-html
     (:form :action ,action
            :method "post"
            ,@(loop for q in questions
                    collect (destructuring-bind (&key ask group type answers) q
                              `(question ,ask (:group ,group :type ,type) ,@answers)))
            (btn-primary (:type "submit") "Submit"))))
