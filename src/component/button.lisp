;; https://getbootstrap.com/docs/5.3/components/buttons/

;; The Bootstrap button component is a versatile and customizable interface
;; element used in numerous contexts across web applications. These buttons are
;; often utilized for various actions and functionalities such as form
;; submissions, toggling visibility of content, triggering modals, and
;; initiating other interactive behaviors.

;; A Bootstrap button is typically defined by an HTML button element or an a
;; element styled with Bootstrap's pre-defined CSS classes.

;; Here are some key features of the Bootstrap button component:

;; Button styles: Bootstrap includes several built-in button styles that
;; indicate different types of actions, including 'primary', 'secondary',
;; 'success', 'danger', 'warning', 'info', 'light', 'dark', and 'link'. These
;; styles control the color of the button.

;; Button sizes: Bootstrap buttons can be resized using the 'btn-lg' or
;; 'btn-sm' classes for larger and smaller buttons, respectively. There's also
;; a 'btn-block' class that makes a button take up the full width of its parent
;; container.

;; Button states: Buttons can also have different states like 'active' and
;; 'disabled'.

;; Outline buttons: Bootstrap also provides 'outline' button styles which have
;; transparent backgrounds and a colored border and text.

(defpackage cl-sbt/btn
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
   :btn-outline-link
   :btn-outline-primary-lg
   :btn-outline-secondary-lg
   :btn-outline-success-lg
   :btn-outline-danger-lg
   :btn-outline-warning-lg
   :btn-outline-info-lg
   :btn-outline-light-lg
   :btn-outline-dark-lg
   :btn-outline-link-lg
   :btn-outline-primary-sm
   :btn-outline-secondary-sm
   :btn-outline-success-sm
   :btn-outline-danger-sm
   :btn-outline-warning-sm
   :btn-outline-info-sm
   :btn-outline-light-sm
   :btn-outline-dark-sm
   :btn-outline-link-sm))

(in-package :cl-sbt/btn)

(defmacro btn ((&key (type "primary") (size "")) &body body)
  "This macro generates a Bootstrap button.

TYPE: (optional) The type of the button (like 'primary', 'secondary', 'success', etc.).
SIZE: (optional) The size of the button ('lg' for large, 'sm' for small).
BODY: The contents of the button.

Example usage:
(btn (:type \"danger\" :size \"lg\") \"Delete\")"
  `(spinneret:with-html
     (:button :type "button"
              :class (concatenate 'string
                                  "btn"
                                  (format nil " btn-~a" ,type)
                                  (if (string-equal ,size "") nil (format nil " btn~a" ,size)))
              ,@body)))

(defmacro define-btn (type &optional (outline nil) (size nil))
  "This macro defines a new macro for creating a Bootstrap button of a specific type, size, and outline style.

TYPE: The type of the button (like 'primary', 'secondary', 'success', etc.).
OUTLINE: (optional) Whether the button should be of the outline style.
SIZE: (optional) The size of the button ('lg' for large, 'sm' for small).

The newly defined macro, when called, will generate HTML for a Bootstrap
button of the specified type and size."
  (let* ((size-name (if (null size) "" (format nil "-~a" size)))
         (outline-name (if (null outline) "" "outline-"))
         (type-name (concatenate 'string outline-name type))
         (macro-name (intern (string-upcase (concatenate 'string "BTN-" outline-name type size-name)))))
    `(defmacro ,macro-name (&body body)
       `(btn (:type ,,type-name :size ,,size-name) ,@body))))

(defmacro define-btns (names)
  "This macro generates a suite of button-creating macros for each provided button type.

NAMES: A list of button type names. Each name should be a string
representing a Bootstrap button type (like 'primary', 'secondary',
'success', etc.).

For each type name in NAMES, this macro defines six new macros: a standard
button, an outline button, a large button, a small button, a large outline
button, and a small outline button.

The newly defined macros, when called, will generate HTML for a Bootstrap
button of the corresponding type, size, and outline style."
  `(progn
     ,@(loop for item in names
             for type-name = (string-downcase (string item))
             collect `(progn
                        (define-btn ,type-name)
                        (define-btn ,type-name t)
                        (define-btn ,type-name t "lg")
                        (define-btn ,type-name t "sm")
                        (define-btn ,type-name nil "lg")
                        (define-btn ,type-name nil "sm")))))

(define-btns (primary secondary success danger warning info light dark link))
