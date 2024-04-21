;; https://getbootstrap.com/docs/5.3/components/spinners/

;; Bootstrap Spinners are versatile components used to indicate a loading state
;; in a variety of scenarios. They can be displayed when the user initiates an
;; action that requires some time to process, such as submitting a form or
;; loading data. They help inform the user that something is happening behind
;; the scenes.

;; Spinners are available in two styles: Border and Grow. The Border style is a
;; rotating border, while the Grow style is a shape that continuously grows and
;; shrinks.

;; Bootstrap allows customization of spinners through CSS classes. You can
;; change the color of the spinner to 'primary', 'secondary', 'success',
;; 'danger', 'warning', 'info', 'light', 'dark'. You can also control the size
;; of the spinner through size classes.

(defpackage dev.metalisp.sbt/component/spinner
  (:use :cl)
  (:export
   :spinner
   :spinner-border-primary
   :spinner-border-secondary
   :spinner-border-success
   :spinner-border-danger
   :spinner-border-warning
   :spinner-border-info
   :spinner-border-light
   :spinner-border-dark
   :spinner-border-primary-sm
   :spinner-border-secondary-sm
   :spinner-border-success-sm
   :spinner-border-danger-sm
   :spinner-border-info-sm
   :spinner-border-warning-sm
   :spinner-border-light-sm
   :spinner-border-dark-sm
   :spinner-grow-primary
   :spinner-grow-secondary
   :spinner-grow-success
   :spinner-grow-danger
   :spinner-grow-info
   :spinner-grow-warning
   :spinner-grow-light
   :spinner-grow-dark
   :spinner-grow-primary-sm
   :spinner-grow-secondary-sm
   :spinner-grow-success-sm
   :spinner-grow-danger-sm
   :spinner-grow-info-sm
   :spinner-grow-warning-sm
   :spinner-grow-light-sm
   :spinner-grow-dark-sm)
  (:documentation "This package provides a set of macros to generate Bootstrap Spinner components."))

(in-package :dev.metalisp.sbt/component/spinner)

(defmacro spinner ((&key (type "border") (color "primary") (size nil)))
  "This macro generates a Bootstrap spinner with a specified type, color, and size.

TYPE: Specifies the spinner style. Can be 'border' or 'grow'. Defaults to 'border'.

COLOR: Specifies the spinner color. Can be 'primary', 'secondary', 'success',
'danger', 'warning', 'info', 'light', 'dark', or 'link'. Defaults to
'primary'.

SIZE: Specifies the size of the spinner. Can be 'sm' for small or NIL for
default size.

Examples:
  (spinner (:type \"border\" :color \"danger\" :size \"sm\"))
  ; Generates a small, red, border spinner

  (spinner (:type \"grow\" :color \"success\"))
  ; Generates a green, growing spinner with default size

  (spinner (:type \"border\" :color \"primary\"))
  ; Generates a blue, border spinner with default size

  (spinner (:type \"grow\" :color \"warning\" :size \"sm\"))
  ; Generates a small, yellow, growing spinner"
  `(spinneret:with-html
     (:div :class ,(if (null size)
                       (format nil "spinner-~a text-~a" type color)
                       (format nil "spinner-~a spinner-~a-~a text-~a" type type size color))
           :role "status"
           (:span :class "visually-hidden" "Loading..."))))

(defmacro define-spinner (type color size)
  "This macro defines a new spinner macro with a specified type, color, and size.

TYPE: Specifies the style of the spinner. It can be 'border' or 'grow'.

COLOR: Specifies the color of the spinner.

SIZE: Specifies the size of the spinner. It can be 'sm' for small or NIL for
default size.

Examples:
  (define-spinner \"border\" \"danger\" \"sm\")
  ; Defines a macro for a small, red, border spinner

  (define-spinner \"grow\" \"success\" NIL)
  ; Defines a macro for a green, growing spinner with default size

  (define-spinner \"border\" \"primary\" NIL)
  ; Defines a macro for a blue, border spinner with default size

  (define-spinner \"grow\" \"warning\" \"sm\")
  ; Defines a macro for a small, yellow, growing spinner"
  (let* ((size-name (if (null size) "" (format nil "-~a" size)))
         (macro-name (intern (string-upcase (concatenate 'string "SPINNER-" type "-" color size-name)))))
    `(defmacro ,macro-name ()
       `(spinner (:type ,,type :color ,,color :size ,,size)))))

(defmacro define-spinners (names)
  "This macro defines a set of new spinner macros.

NAMES: A list of colors to use for the spinners.

Example:
  (define-spinners (\"primary\" \"secondary\" \"success\" \"danger\" \"warning\" \"info\" \"light\" \"dark\"))
  ; Defines 32 new macros (8 colors * 2 types * 2 sizes)"
  `(progn
     ,@(loop for item in names
             for color = (string-downcase (string item))
             collect `(progn
                        (define-spinner "border" ,color nil)
                        (define-spinner "grow" ,color nil)
                        (define-spinner "border" ,color "sm")
                        (define-spinner "grow" ,color "sm")))))

(define-spinners (primary secondary success danger warning info light dark))
