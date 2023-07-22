;; Bootstrap utilities are a set of CSS classes designed to assist in the
;; fine-tuning of various aspects of a web layout. The aim is to give
;; developers precise control over visual elements such as spacing, color,
;; opacity, sizing, overflow behavior, and many other facets of web design,
;; without the need to write custom CSS rules for every situation.

;; These utility classes are designed to be semantic and easy to understand.
;; They provide a common language for developers, allowing team members to
;; quickly grasp the purpose of a class without needing to refer to the CSS
;; definitions.

;; Here's a brief overview of some of the key utility areas in Bootstrap:

;; Spacing Utilities: Bootstrap provides classes for modifying the margin and
;; padding of elements. These classes allow for changes to be applied uniformly
;; around an element, or targeted to specific sides.

;; Color Utilities: These classes allow developers to easily apply background
;; color to elements, and they're based on the theme colors defined in
;; Bootstrap's Sass variables.

;; Opacity Utilities: With these classes, developers can easily control the
;; transparency of an element without modifying its color properties directly.

;; Overflow Utilities: Bootstrap provides utilities to control how content
;; overflows its container. These classes can be used to create scrollable
;; areas, hide overflowing content, or automatically wrap overflow.

;; Sizing Utilities: These utilities offer control over an element's width and
;; height. They can be absolute (pixels), relative (%), or based on viewport
;; height/width.

;; Text Utilities: These classes provide a way to change text alignment,
;; wrapping, weight, color, and more. They're useful for managing the
;; appearance of typography across different viewport sizes.

;; Display Utilities: These classes control the CSS display property of
;; elements, allowing for easy adjustments to visibility and layout across
;; different screen sizes.

(defpackage :cl-sbt/utility
  (:use :cl)
  (:export
   :background
   :color
   :opacity
   :overflow
   :sizing
   :spacing
   :text
   :valign)
  (:documentation "A module for generating Bootstrap utility classes."))

(in-package :cl-sbt/utility)

(defun string-clean (str)
  (string-trim " " (string-downcase str)))

(defun background (&key (color nil) (gradient nil))
  "Generates a Bootstrap background class.

COLOR: Specifies the color, should be :primary, :secondary, :success,
:danger, :warning, :info, :light, :dark, :body, :muted, :white, :transparent,
or nil (default color).

GRADIENT: Specifies if the background should have a gradient, should be :t or
:nil. If :t, it will add a 'bg-gradient' to the class string.

Example 1:
  (background :color :primary)
  ; This will generate a string 'bg-primary'

Example 2:
  (background :color :danger :gradient :t)
  ; This will generate a string 'bg-danger bg-gradient'

Example 3:
  (background :color :light)
  ; This will generate a string 'bg-light'

Example 4:
  (background :color :dark :gradient :t)
  ; This will generate a string 'bg-dark bg-gradient'"
  (let ((color-str (if (null color) "" (format nil "bg-~a" color)))
        (gradient-str (if (eq gradient :t) " bg-gradient" "")))
    (string-clean (concatenate 'string color-str gradient-str))))

(defun color (&key (text nil) (background nil))
  "Generates a Bootstrap color class.

TEXT: Specifies the color of the text. Should be 'primary', 'secondary',
'success', 'danger', 'warning', 'info', 'light', 'dark', 'white', 'body',
'muted', 'black-50', 'white-50', 'reset'.

BACKGROUND: Specifies the color of the background. Should be 'primary',
'secondary', 'success', 'danger', 'warning', 'info', 'light', 'dark', 'white',
'transparent'.

Example 1:
  (color :text :primary)
  ; This will generate a string 'text-primary'

Example 2:
  (color :background '(:color :danger))
  ; This will generate a string 'bg-danger'

Example 3:
  (color :text :info :background '(:color :dark))
  ; This will generate a string 'text-info bg-dark'

Example 4:
  (color :text :white :background '(:color :primary))
  ; This will generate a string 'text-white bg-primary'"
  (string-clean (concatenate 'string
                             (if (null text) "" (format nil "text-~a " text))
                             (if (null background) "" (apply #'background background)))))

(defun opacity (&key (level nil))
  "Generates a Bootstrap opacity class.

LEVEL: Specifies the opacity level, should be a number from 0 to 100, or :auto.

Example 1:
  (opacity :level 25)
  ; This will generate a string 'opacity-25'

Example 2:
  (opacity :level 50)
  ; This will generate a string 'opacity-50'

Example 3:
  (opacity :level 75)
  ; This will generate a string 'opacity-75'

Example 4:
  (opacity :level :auto)
  ; This will generate a string 'opacity-auto'"
  (let ((level-str (if (null level) "" (if (eq level :auto)
                                           "opacity-auto"
                                           (format nil "opacity-~d" level)))))
    (string-clean (concatenate 'string level-str))))

(defun overflow (&key (direction nil) (value nil))
  "Generates a Bootstrap overflow class.

DIRECTION: Specifies the direction, should be :x, :y, or nil (both directions).

VALUE: Specifies the overflow value, should be :auto, :hidden, :visible,
:scroll, or nil (default value).

Example 1:
  (overflow :direction :x :value :auto)
  ; This will generate a string 'overflow-x-auto'

Example 2:
  (overflow :direction :y :value :hidden)
  ; This will generate a string 'overflow-y-hidden'

Example 3:
  (overflow :value :visible)
  ; This will generate a string 'overflow-visible'

Example 4:
  (overflow :direction :x :value :scroll)
  ; This will generate a string 'overflow-x-scroll'"
  (let* ((dir-str (if (null direction)
                      ""
                      (format nil "overflow-~a" direction)))
         (value-str (if (null value)
                        ""
                        (if (string= dir-str "")
                            (format nil "overflow-~a" value)
                            (format nil "-~a" value)))))
    (string-clean (concatenate 'string dir-str value-str))))

(defun sizing (&key (direction nil) (size nil))
  "Generates a Bootstrap sizing class.

DIRECTION: Specifies the direction, should be :w or :h.

SIZE: Specifies the size, should be a number from 0 to 100, :25, :50, :75,
:100, :auto, or nil (default size).

Example 1:
  (sizing :direction :w :size 50)
  ; This will generate a string 'w-50'

Example 2:
  (sizing :direction :h :size :auto)
  ; This will generate a string 'h-auto'

Example 3:
  (sizing :direction :w :size :100)
  ; This will generate a string 'w-100'

Example 4:
  (sizing :direction :h :size 75)
  ; This will generate a string 'h-75'"
  (let* ((dir-str (if (null direction) "" (format nil "~a-" direction)))
         (size-str (if (null size) ""
                       (if (eq size :auto)
                           "auto"
                           (if (numberp size)
                               (format nil "~d" size)
                               (format nil "~a" size))))))
    (string-clean (concatenate 'string dir-str size-str))))

(defun spacing (&key (property nil) (side nil) (size nil) (breakpoint nil))
  "Generates a Bootstrap spacing class.

PROPERTY: Specifies the property, should be :m (margin) or :p (padding).

SIDE: Specifies the side, should be :t (top), :b (bottom), :s (start),
:e (end), :x (horizontal), :y (vertical), or nil (all sides).

SIZE: Specifies the size, should be a number from 0 to 5, or :auto.

BREAKPOINT: Specifies the breakpoint, should be :xs, :sm, :md, :lg, :xl, or
:xxl, or nil (all breakpoints).

Example 1:
  (spacing :property :m :side :t :size 3 :breakpoint :md)
  ; This will generate a string 'mt-md-3'

Example 2:
  (spacing :property :p :side :b :size 2 :breakpoint :lg)
  ; This will generate a string 'pb-lg-2', which represents a large breakpoint
  ; with bottom padding of size 2.

Example 3:
  (spacing :property :m :size :auto)
  ; This will generate a string 'm-auto', which sets auto margin on all sides
  ; for all breakpoints.

Example 4:
  (spacing :property :p :side :x :size 5)
  ; This will generate a string 'px-5', which sets horizontal padding of size 5
  ; for all breakpoints."
  (let ((property-str (if (null property) "" (format nil "~a" property)))
        (side-str (if (null side) "" (format nil "~a" side)))
        (size-str (if (null size) "" (if (eq size :auto) "auto" (format nil "~d" size))))
        (breakpoint-str (if (null breakpoint) "" (format nil "~a" breakpoint))))
    (string-clean
     (concatenate 'string
                  property-str
                  side-str
                  (if breakpoint "-" "") breakpoint-str
                  (if size "-" "") size-str))))

(defun text (&key (alignment nil) (transform nil) (weight nil) (wrap nil) (monospace nil))
  "Generates a Bootstrap text utility class.

ALIGNMENT: Specifies the text alignment. Should be 'start', 'end', 'center'.

TRANSFORM: Specifies the text transformation. Should be 'lowercase',
'uppercase', 'capitalize'.

WEIGHT: Specifies the text weight. Should be 'bold', 'bolder', 'normal',
'light', 'lighter'.

MONOSPACE: If true, sets the font to monospace.

Example 1:
  (text :alignment :start)
  ; This will generate a string 'text-start'

Example 2:
  (text :transform :uppercase)
  ; This will generate a string 'text-uppercase'

Example 3:
  (text :weight :bold :monospace t)
  ; This will generate a string 'fw-bold font-monospace'

Example 4:
  (text :alignment :center :transform :lowercase)
  ; This will generate a string 'text-center text-lowercase'

Example 5:
  (text :alignment :end :weight :light :monospace t)
  ; This will generate a string 'text-end fw-light font-monospace '

Example 6:
  (text :transform :capitalize :wrap :wrap)
  ; This will generate a string 'text-capitalize text-wrap '

Example 7:
  (text :alignment :center :transform :uppercase :weight :bolder :wrap :nowrap :monospace t)
  ; This will generate a string 'text-center text-uppercase fw-bolder
  ; text-nowrap font-monospace '"
  (let ((alignment-str (if (null alignment) "" (format nil "text-~a " alignment)))
        (transform-str (if (null transform) "" (format nil "text-~a " transform)))
        (weight-str (if (null weight) "" (format nil "fw-~a " weight)))
        (wrap-str (if (null wrap) "" (format nil "text-~a " wrap)))
        (monospace-str (if (null monospace) "" "font-monospace")))
    (string-clean (concatenate 'string
                               alignment-str
                               transform-str
                               weight-str
                               wrap-str
                               monospace-str))))

(defun valign (&key (align nil))
  "Generates a Bootstrap vertical align class.

ALIGN: Specifies the alignment, should be :baseline, :top, :middle, :bottom,
:text-bottom, :text-top or nil (default alignment).

Example 1:
  (valign :align :baseline)
  ; This will generate a string 'align-baseline'

Example 2:
  (valign :align :top)
  ; This will generate a string 'align-top'

Example 3:
  (valign :align :middle)
  ; This will generate a string 'align-middle'"
  (let ((align-str (if (null align) "" (format nil "align-~a" align))))
    (string-clean align-str)))
