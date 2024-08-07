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

(defpackage ml-sbt/list-group
  (:use :cl)
  (:export
   :item
   :with-list-group)
(:documentation "A Common Lisp package for generating Bootstrap List Group components."))

(in-package :ml-sbt/list-group)

(defmacro item (&body body)
  "This macro generates a Bootstrap list group item.

BODY: The contents of the list group item. It should be a sequence of forms
which will be used as the contents of the list group item.

Example:
  (item \"This is a list group item\")
  (item (:p \"This is a list group item\"))"
  `(spinneret:with-html
     (:li :class "list-group-item" ,@body)))

(defmacro with-list-group (items &optional flush)
  "This macro generates a Bootstrap list group.

ITEMS: A list of items to be included in the list group.

FLUSH: If true, adds the 'list-group-flush' class."
  (let ((class-str (concatenate 'string "list-group" (when flush " list-group-flush"))))
    `(spinneret:with-html
       (:ul :class ,class-str
            ,@(if (and (listp items) (eq (car items) 'quote))
                  ;; If items is a quoted list, use it directly
                  (loop for item in (cadr items)
                        collect `(:li :class "list-group-item" ,item))
                  ;; Otherwise, assume it's a variable and generate code to be evaluated at runtime
                  ;; FUCK YOU COMMON LISP! I really don't understand how this works. :(
                  ;; If you are able to understand it, and able to explain it to me, please send me an email:
                  ;; marcus.kammer@mailbox.org
                  `((loop for item in ,items
                          do (:li :class "list-group-item" item))))))))
