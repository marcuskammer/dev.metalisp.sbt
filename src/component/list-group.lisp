;; List groups
;; Create lists of content in a card with a flush list group.

(defpackage cl-sbt-list-group
  (:use :cl)
  (:export
   :item
   :list-group))

(in-package :cl-sbt-list-group)

(defmacro item (&body body)
  "This macro generates a Bootstrap list group item.

  BODY: The contents of the list group item."

  `(spinneret:with-html
     (:li :class "list-group-item" ,@body)))

(defmacro list-group (&rest rest)
  "This macro generates a Bootstrap list group.

  REST: A sequence of items to be included in the list group. Each item is a keyword-value pair, where the keyword is ':content' and the value is the content of the item."

  `(spinneret:with-html
     (:ul :class "list-group list-group-flush"
          ,@(loop for item in rest
	          collect (destructuring-bind (&key content) item
	        	    `(item ,content))))))
