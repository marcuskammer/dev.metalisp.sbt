;; The Bootstrap Dropdown component is a toggleable, contextual menu for
;; displaying lists of links and more. It's made interactive with the inclusion of
;; JavaScript, which allows it to convert a button into a dropdown menu.

;; Key Features:

;; Toggleable: It can be toggled to display or hide its list of links.

;; Contextual: The dropdown can be used in various contexts, such as navigation
;; bars, tabs, and buttons.

;; Flexible: Dropdowns can contain a wide variety of elements, not just links. For
;; example, you could include form elements or other complex sets of content.

;; Customizable: The appearance and behavior of the dropdown can be customized
;; with Bootstrap's built-in classes.

;; In terms of structure, a Bootstrap dropdown typically consists of a button with
;; data-bs-toggle="dropdown" attribute to enable the dropdown functionality, and a
;; div or ul with class .dropdown-menu to create the actual dropdown menu. Each
;; item in the dropdown is typically enclosed in an a or button element with the
;; class .dropdown-item. Additionally, dropdowns can be made to expand
;; upwards (instead of downwards) by adding the .dropup class.

(defpackage cl-sbt-dropdown
  (:use :cl)
  (:export
   :menu
   :item
   :dropdown))

(in-package :cl-sbt-dropdown)

(defmacro menu (&body body)
  `(spinneret:with-html
     (:ul :class "dropdown-menu"
          ,@body)))

(defmacro item (&body body)
  `(spinneret:with-html
     (:li (:a :class "dropdown-item" :href "#" ,@body))))

(defmacro dropdown ((&key (title "")) &body body)
  `(spinneret:with-html
     (:div :class "dropdown"
           (:button :class "btn btn-secondary dropdown-toggle"
                    :type "button"
                    :data-bs-toggle "dropdown"
                    :aria-expanded "false"
                    ,title)
           ,@body)))
