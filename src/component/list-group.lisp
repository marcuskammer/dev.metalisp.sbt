;; https://getbootstrap.com/docs/5.3/components/list-group/

;; Bootstrap's List Group component is a flexible and versatile component used
;; for displaying a series of content which can be a mix of various types, like
;; text, links, buttons, etc., in a list format. This component is typically
;; used for showing a list of items within a card, a sidebar, or a dropdown
;; menu.

;; The component is highly customizable, allowing for different styles and
;; effects. By default, the List Group items are styled as plain text, but can
;; be modified into clickable elements or buttons. The List Group component
;; also supports contextual classes for styling list items with different
;; states, such as success, warning, danger, and so on.

;; In addition to this, the List Group component supports adding badges to list
;; items, making it suitable for a wide range of applications, such as a list
;; of emails with the number of unread messages, a list of tasks with their
;; completion status, and so on.

(defpackage cl-sbt/list-group
  (:use :cl)
  (:export
   :item
   :list-group)
(:documentation "A Common Lisp package for generating Bootstrap List Group components."))

(in-package :cl-sbt/list-group)

(defmacro item (&body body)
  "This macro generates a Bootstrap list group item.

BODY: The contents of the list group item. It should be a sequence of forms
which will be used as the contents of the list group item.

Example:
  (item \"This is a list group item\")
  (item (:p \"This is a list group item\"))"
  `(spinneret:with-html
     (:li :class "list-group-item" ,@body)))

(defmacro list-group (&rest rest)
  "This macro generates a Bootstrap list group.

REST: A sequence of items to be included in the list group. Each item is a
keyword-value pair, where the keyword is ':content' and the value is the
content of the item.

Example:
  (list-group (:content \"First item\") (:content \"Second item\"))"
  `(spinneret:with-html
     (:ul :class "list-group list-group-flush"
          ,@(loop for item in rest
	          collect (destructuring-bind (&key content) item
	        	    `(item ,content))))))
