;;; cl-sbt/src/pattern/questionnaire.lisp -- Questionnaire and Choice Generation for HTML forms

;;; Commentary:

;; This package, `cl-sbt/questionnaire`, provides a suite of utilities for generating
;; HTML forms composed of multiple questions. Each question can have a different type
;; of input (radio buttons, checkboxes, etc.), and the form itself can be customized
;; according to the target URL for submissions.
;;
;; This package relies on the `spinneret` package for HTML generation and imports utility
;; functions and macros from `cl-sbt/btn` for button styling, `cl-sbt/utility` for additional
;; CSS utility, and `cl-sbt/form` for form-related functionality.
;;
;; The package provides the following public interfaces:
;; - `question`: A macro to generate a single question with multiple choices.
;; - `questionnaire`: A macro similar to `questionnaire-1`, but more dynamic in accepting questions.

;;; Code:

(defpackage cl-sbt/questionnaire
  (:use
   :cl)
  (:import-from
   :cl-sbt/btn
   :btn-primary)
  (:import-from
   :cl-sbt/utility
   :spacing)
  (:import-from
   :cl-sbt/form
   :find-l10n
   :checkable
   :ctrl-1)
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

(defun choose-input-form (type group item)
  (cond ((or (string= type "radio")
             (string= type "checkbox"))
         (checkable type group item))
        ((string= type "text")
         (ctrl-1 type group item))))

(defmacro question (ask group &rest choices)
  "This macro generates a fieldset for a question with multiple answers.

ASK: The text of the question to be displayed in the legend.

GROUP: Specifies the name attribute for the input elements.

CHOICES: A list of strings starting with a keyword representing the different
answers available for selection.

Example:
  (question \"How old are you?\" \"age\" (:radio \"18-24\" \"25-34\" \"35-44\")"
  `(spinneret:with-html
     (:fieldset (:legend ,ask)
                (:ol ,@(loop for choice in choices
                             append (multiple-value-bind (type choices)
                                        (resolve-input-and-choices choice)
                                      (loop for item in choices
                                            collect `(:li (choose-input-form ,type ,group ,item))))))
                (:hr :class (spacing :property "m" :side "y" :size 4)))))

(defun split-plist-by-keyword (plist)
  (let ((result '())
        (current-list '()))
    (loop for item in plist
          do (if (keywordp item)
                 (progn
                   (when current-list
                     (push (nreverse current-list) result))
                   (setq current-list (list item)))
                 (push item current-list)))
    (when current-list
      (push (nreverse current-list) result))
    (nreverse result)))

(defmacro questionnaire (action &rest questions)
  "This macro generates an HTML form composed of multiple questions, each
   rendered using the `question` macro.

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
            ,@(loop for q in questions
                    collect (destructuring-bind (&key ask group choices) q
                              (let ((splitted-choices (split-plist-by-keyword choices)))
                                `(question ,ask ,group ,@splitted-choices))))
            (btn-primary (:type "submit")
              (find-l10n "submit" spinneret:*html-lang*)))))
