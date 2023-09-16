;;;; questionnaire.lisp

;;;; This package, `cl-sbt/questionnaire`, provides a suite of utilities for generating
;;;; HTML forms composed of multiple questions. Each question can have a different type
;;;; of input (radio buttons, checkboxes, etc.), and the form itself can be customized
;;;; according to the target URL for submissions.

(defpackage cl-sbt/questionnaire
  (:documentation "Defines the `cl-sbt/questionnaire` package, a utility suite for generating HTML forms with various types of questions.")
  (:use
   :cl)
  (:import-from
   :cl-sbt
   :l10n
   :find-l10n)
  (:import-from
   :cl-sbt/btn
   :btn-primary)
  (:import-from
   :cl-sbt/utility
   :spacing)
  (:import-from
   :cl-sbt/form
   :checkable
   :ctrl)
  (:export
   :question
   :questionnaire))

(in-package :cl-sbt/questionnaire)

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

(defun resolve-input-and-choices (choices)
  "Separate the input type keyword from the remaining CHOICES in a list.

If the first element of CHOICES is a keyword, it is taken to be the input type
keyword, and the rest of the list is taken to be the actual choices.

CHOICES: The choices list, possibly including an input type keyword.

Returns two values:
  1. The input type string if a keyword is present, or NIL if no keyword is found.
  2. The remaining choices in the list, excluding the input type keyword."
  (let ((input-type-keyword (first choices)))
    (if (keywordp input-type-keyword)
        (values (resolve-input-type (string-downcase input-type-keyword)) (rest choices))
        (values nil choices))))

(defun choose-input-form (type)
  "Choose the appropriate function to generate the HTML form input based on TYPE.

TYPE: A string specifying the HTML input type like 'radio', 'checkbox', 'text',
etc.

Returns:
  A function that can be used to generate the HTML form input."
  (cond
    ((string= type "radio") #'checkable)
    ((string= type "checkbox") #'checkable)
    ((string= type "text") #'ctrl)
    (t (error "Unknown type ~A" type))))

(defun apply-input-form (type group item)
  "Apply the chosen input form function to generate HTML for a single form
   element.

TYPE: A string specifying the HTML input type like 'radio', 'checkbox', 'text',
etc.

GROUP: Specifies the name attribute for the input elements.

ITEM: The particular choice item that this form element represents.

Returns:
  The HTML form element generated for the ITEM."
  (funcall (choose-input-form type) type group item))

(defmacro process-choice (group choice)
  (multiple-value-bind (type inputs)
      (resolve-input-and-choices choice)
    (if (string= type "combo")
        `(spinneret:with-html
           (:li (select () ,@inputs)))
        `(spinneret:with-html
           ,@(loop for input in inputs
                   collect
                   `(:li (apply-input-form ,type ,group ,input)))))))

(defmacro question (ask group &body body)
  "This macro generates a fieldset for a question with multiple answers.

ASK: The text of the question to be displayed in the legend.

GROUP: Specifies the name attribute for the input elements.

BODY: A list of strings starting with a keyword representing the different
answers available for selection.

Example:
  (question \"How old are you?\" \"age\" (:radio \"18-24\" \"25-34\" \"35-44\"))"
  `(spinneret:with-html
     (:fieldset (:legend ,ask)
                (:ol ,@(loop for choice in body
                             append
                             `((process-choice ,group ,choice))))
                (:hr :class (spacing :property "m" :side "y" :size 4)))))

(defun split-plist-by-keyword (plist)
  "Splits a property list (PLIST) into a list of smaller property lists, each starting with a keyword.

PLIST: A property list that includes keywords and their associated values.

This function treats the first element after each keyword as its value, and
each new keyword signifies the start of a new property list.

Example:
  Given the plist '(:a 1 :b 2 :c 3 :d 4),
  it will return '((:a 1) (:b 2) (:c 3) (:d 4)).

Returns:
  A list of property lists, each starting with a keyword."
  ;; Initialize result and current-list
  (let ((result '())
        (current-list '()))
    ;; Loop through each item in plist
    (loop for item in plist
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

(defun extract-question-components (question)
  "Extracts components of a question stored as a plist.

QUESTION: A plist representing a question.

Returns multiple values:
  - The question text (ASK)
  - The group name (GROUP)
  - The choices (CHOICES)"
  (let ((splitted-list (split-plist-by-keyword question)))
    (apply #'values (mapcar (lambda (x) (first (last x))) splitted-list))))

(defmacro questionnaire (action &body body)
  "This macro generates an HTML form composed of multiple questions, each rendered using the `question` macro.

ACTION: Specifies the URL where the form will be submitted. This should be a
string representing the URL path.

QUESTIONS: A series of plists, each representing a question. Each plist should
contain the keys :ask, :group, and :choices. The first element of :choices
should be a keyword specifying the type of input elements (e.g. :radio),
followed by a list of answer options.

Example 1:
  (questionnaire \"/submit\"
                 (:ask \"How old are you?\"
                  :group \"age\"
                  :choices (:radio \"18-24\" \"25-34\" \"35-44\")))
Example 2:
  (questionnaire \"/submit\"
                 (:ask \"How old are you?\"
                  :group \"age\"
                  :choices (:single \"18-24\" \"25-34\" \"35-44\")))
Example 3:
  (questionnaire \"/submit\"
                 (:ask \"Which social media platforms do you use regularly?\"
                  :group \"age\"
                  :choices (:multiple \"Facebook\" \"Twitter\" \"Instagram\")))
Example 4:
  (questionnaire \"/submit\"
                 (:ask \"Which social media platforms do you use regularly?\"
                  :group \"age\"
                  :choices (:multiple \"Facebook\" \"Twitter\" \"Instagram\" :text \"Others\")))"
  `(spinneret:with-html
     (:form :action ,action
            :method "post"
            :class (spacing :property "p" :side "y" :size 5)
            ,@(loop for q in body
                    collect (multiple-value-bind (ask group choices)
                                (extract-question-components q)
                              (let ((splitted-choices (split-plist-by-keyword choices)))
                                `(question ,ask ,group ,@splitted-choices))))
            (btn-primary (:type "submit")
              (find-l10n "submit" spinneret:*html-lang* l10n)))))
