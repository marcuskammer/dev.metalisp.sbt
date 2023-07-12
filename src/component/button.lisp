;; https://getbootstrap.com/docs/5.3/components/buttons/

;; The Bootstrap button component is a versatile and customizable interface
;; element used in numerous contexts across web applications. These buttons are
;; often utilized for various actions and functionalities such as form
;; submissions, toggling visibility of content, triggering modals, and initiating
;; other interactive behaviors.

;; A Bootstrap button is typically defined by an HTML button element or an a
;; element styled with Bootstrap's pre-defined CSS classes.

;; Here are some key features of the Bootstrap button component:

;; Button styles: Bootstrap includes several built-in button styles that indicate
;; different types of actions, including 'primary', 'secondary', 'success',
;; 'danger', 'warning', 'info', 'light', 'dark', and 'link'. These styles control
;; the color of the button.

;; Button sizes: Bootstrap buttons can be resized using the 'btn-lg' or 'btn-sm'
;; classes for larger and smaller buttons, respectively. There's also a
;; 'btn-block' class that makes a button take up the full width of its parent
;; container.

;; Button states: Buttons can also have different states like 'active' and 'disabled'.

;; Outline buttons: Bootstrap also provides 'outline' button styles which have
;; transparent backgrounds and a colored border and text.

(defpackage cl-sbt-btn
  (:use :cl)
  (:export
   :btn
   :btn-primary
   :btn-secondary
   :btn-success
   :btn-danger
   :btn-warning
   :btn-info
   :btn-light
   :btn-dark
   :btn-link
   :btn-primary-lg
   :btn-secondary-lg
   :btn-success-lg
   :btn-danger-lg
   :btn-warning-lg
   :btn-info-lg
   :btn-light-lg
   :btn-dark-lg
   :btn-link-lg
   :btn-primary-sm
   :btn-secondary-sm
   :btn-success-sm
   :btn-danger-sm
   :btn-warning-sm
   :btn-info-sm
   :btn-light-sm
   :btn-dark-sm
   :btn-link-sm
   :btn-outline-primary
   :btn-outline-secondary
   :btn-outline-success
   :btn-outline-danger
   :btn-outline-warning
   :btn-outline-info
   :btn-outline-light
   :btn-outline-dark
   :btn-outline-link))

(in-package :cl-sbt-btn)

(defmacro btn ((&key (type nil) (size nil)) &body body)
  "This macro generates a Bootstrap button.

   TYPE: (optional) The type of the button (like 'primary', 'secondary', 'success', etc.).
   SIZE: (optional) The size of the button ('lg' for large, 'sm' for small).
   BODY: The contents of the button."

  `(spinneret:with-html
     (:button :type "button"
              :class (concatenate 'string
                                  "btn"
                                  (if (null ,type) nil (format nil " btn-~a" ,type))
                                  (if (null ,size) nil (format nil " btn-~a" ,size)))
              ,@body)))

(defmacro define-btns (names)
  "This macro generates specific button macros based on the provided names.

   NAMES: A list of button type names. For each name in this list, three macros will be generated: a standard button, a large button, and a small button."

  `(progn
     ,@(loop for item in names
             for symbol = (intern (concatenate 'string "BTN-" (symbol-name item)))
             for symbol-lg = (intern (concatenate 'string "BTN-" (symbol-name item) "-LG"))
             for symbol-sm = (intern (concatenate 'string "BTN-" (symbol-name item) "-SM"))
             for symbol-outline = (intern (concatenate 'string "BTN-OUTLINE-" (symbol-name item) "-SM"))
             for item-name = (string-downcase (format nil "~a" item))
             collect `(progn
                        (defmacro ,symbol (&body body)
                          `(btn (:type ,(string ',item-name)) ,@body))
                        (defmacro ,symbol-lg (&body body)
                          `(btn (:type ,(string ',item-name) :size "lg") ,@body))
                        (defmacro ,symbol-sm (&body body)
                          `(btn (:type ,(string ',item-name) :size "sm") ,@body))
                        (defmacro ,symbol-outline (&body body)
                          `(btn (:type ,(string ',item-name)) ,@body))))))

(define-btns (primary secondary success danger warning info light dark link))
