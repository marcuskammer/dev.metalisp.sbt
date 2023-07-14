(defpackage cl-sbt-spinner
  (:use :cl)
  (:export
   :spinner))

(in-package :cl-sbt-spinner)

(defmacro spinner ((&key (type "border") (color "primary")))
  "This macro generates a Bootstrap spinner with a specified color.

TYPE: Specifies the spinner style. Can be 'border' or 'grow'. Defaults to 'border'.
COLOR: Specifies the spinner color. Can be 'primary', 'secondary', 'success', 'danger', 'warning', 'info', 'light', 'dark', or 'link'. Defaults to 'primary'."
  `(spinneret:with-html
     (:div :class (format nil "spinner-~a text-~a" ,type ,color)
           :role "status"
           (:span :class "visually-hidden" "Loading..."))))

(defmacro define-spinner (type color)
  "This macro defines a new spinner macro with a specified style and color.

TYPE: Specifies the style of the spinner. It can be 'border' or 'grow'.
COLOR: Specifies the color of the spinner."
  (let ((macro-name (intern (string-upcase (concatenate 'string "SPINNER-" type "-" color)))))
    `(defmacro ,macro-name ()
       `(spinner (:type ,,type :color ,,color)))))

(defmacro define-spinners (names)
  "This macro defines a set of new spinner macros.

NAMES: A list of colors to use for the spinners."
  `(progn
     ,@(loop for item in names
             for color = (string-downcase (string item))
             collect `(progn
                        (define-spinner "border" ,color)
                        (define-spinner "grow" ,color)))))

(define-spinners (primary secondary success danger warning info light dark link))
