;; https://getbootstrap.com/docs/5.3/components/dropdowns/

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

(defpackage dev.metalisp.sbt/component/dropdown
  (:use :cl)
  (:export
   :menu
   :item
   :dropdown))

(in-package :dev.metalisp.sbt/component/dropdown)

(defmacro menu (&body body)
  "This macro generates a Bootstrap dropdown menu.

BODY: The contents of the menu, which should typically be created using the
`item` macro.

Example:
  (menu (item \"Option 1\") (item \"Option 2\"))
  ; This generates a dropdown menu with two options: 'Option 1' and 'Option 2'."
  `(spinneret:with-html
     (:ul :class "dropdown-menu"
          ,@body)))

(defmacro item (&body body)
  "This macro generates a Bootstrap dropdown item.

BODY: The text for the dropdown item.

Example:
  (item \"Option 1\")
  ; This generates a dropdown item with the text 'Option 1'."
  `(spinneret:with-html
     (:li (:a :class "dropdown-item" :href "#" ,@body))))

(defmacro dropdown ((&key (name "")) &body body)
  "This macro generates a Bootstrap dropdown component.

NAME: The text for the dropdown button.

BODY: The contents of the dropdown, which should be created using the `menu`
macro.

Example:
  (dropdown (:name \"Choose option\")
    (menu
      (item \"Option 1\")
      (item \"Option 2\")))
  ; This generates a dropdown button with the text 'Choose option' and a
  ; dropdown menu with two options."
  `(spinneret:with-html
     (:div :class "dropdown"
           (:button :class "btn btn-secondary dropdown-toggle"
                    :type "button"
                    :data-bs-toggle "dropdown"
                    :aria-expanded "false"
                    ,name)
           ,@body)))
