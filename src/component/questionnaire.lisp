(defpackage cl-sbt/questionnaire
  (:use
   :cl)
  (:import-from
   :cl-sbt/btn
   :btn-primary)
  (:export
   :choice
   :question
   :questionnaire))

(in-package :cl-sbt/questionnaire)

(defmacro choice (text name type)
  "This macro generates a list item for an answer option in a question.

TEXT: The display text of the answer option.

NAME: Specifies the name attribute for the input element. This should align
with the name specified in the containing question.

TYPE: Specifies the type of input element, such as \"radio\" for radio buttons.

Example usage:
  (choice \"18-24\" \"age\" \"radio\")"
  `(spinneret:with-html
     (:li (:label :class "form-label"
                  (:input :type ,type :name ,name)
                  (format nil " ~a" ,text)))))

(defmacro question (question (&key (group "group") (type "radio")) &rest choices)
  "This macro generates a fieldset for a question with multiple answers.

QUESTION: The text of the question to be displayed in the legend.

NAME: Specifies the name attribute for the input elements.

TYPE: Specifies the type of input elements. Commonly used value is \"radio\".

CHOICES: A list of strings representing the different answers available for
selection.

Example:
  (question \"How old are you?\"
            (:group \"age\" :type \"radio\") \"18-24\" \"25-34\" \"35-44\")"
  `(spinneret:with-html
     (:fieldset (:legend ,question)
                (:ol ,@(loop for text in choices
                             collect `(choice ,text ,group ,type))))))

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
contain the keys :ask, :group, and :choices. The first element of :choices
should be a keyword specifying the type of input elements (e.g. :radio), followed
by a list of answer options.

Example:
  (questionnaire-1 \"/submit\"
                   (:ask \"How old are you?\" :group \"age\" :choices (:radio \"18-24\" \"25-34\" \"35-44\")))"
  `(spinneret:with-html
     (:form :action ,action
            :method "post"
            ,@(loop for q in questions
                    collect (destructuring-bind (&key ask group choices) q
                              (let ((input-type (string-downcase (when (keywordp (first choices))
                                                                   (pop choices)))))
                                `(question ,ask (:group ,group :type ,input-type) ,@choices))))
            (btn-primary (:type "submit") "Submit"))))
