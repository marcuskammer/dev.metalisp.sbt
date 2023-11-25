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

(defpackage :dev.metalisp.sbt/utility
  (:use :cl)
  (:export
   :*colors*
   :*breakpoints*
   :*sides*
   :background
   :color
   :opacity
   :overflow
   :sizing
   :spacing
   :text
   :valign)
  (:documentation "A module for generating Bootstrap utility classes."))

(in-package :dev.metalisp.sbt/utility)

(defparameter *colors*
  '("body" "secondary" "tertiary" "emphasis" "border" "primary" "success" "danger" "warning" "info" "light" "dark" )
  "List of color classes and variables defined in the updated Bootstrap framework.

  https://getbootstrap.com/docs/5.3/customize/color/

  - 'body': Default foreground (color) and background, including components.
    DARK THEME:
    --bs-body-color: #dee2e6
    --bs-body-color-rgb: 222,226,230
    --bs-body-bg: #212529
    --bs-body-bg-rgb: 33,37,41
    LIGHT THEME:
    --bs-body-color: #212529
    --bs-body-color-rgb: 33,37,41
    --bs-body-bg: #fff
    --bs-body-bg-rgb: 255,255,255

  - 'secondary': Use the color option for lighter text. Use the bg option for
  dividers and to indicate disabled component states.
    DARK THEME:
    --bs-secondary-color: rgba(222, 226, 230, 0.75)
    --bs-secondary-color-rgb: 222,226,230;
    --bs-secondary-bg: #343a40
    --bs-secondary-bg-rgb: 52,58,64
    LIGTH THEME:
    --bs-secondary-color: rgba(33, 37, 41, 0.75)
    --bs-secondary-color-rgb: 33,37,41
    --bs-secondary-bg: #e9ecef
    --bs-secondary-bg-rgb: 233,236,239

  - 'tertiary': Use the color option for even lighter text. Use the bg option
  to style backgrounds for hover states, accents, and wells.
    DARK THEME:
    --bs-tertiary-color: rgba(222, 226, 230, 0.5)
    --bs-tertiary-color-rgb: 222,226,230
    --bs-tertiary-bg: #2b3035
    --bs-tertiary-bg-rgb: 43,48,53
    LIGHT THEME:
    --bs-tertiary-color: rgba(33, 37, 41, 0.5)
    --bs-tertiary-color-rgb: 33,37,41
    --bs-tertiary-bg: #f8f9fa
    --bs-tertiary-bg-rgb: 248,249,250

  - 'emphasis': For higher contrast text. Not applicable for backgrounds.
    DARK THEME:
    --bs-emphasis-color: #fff
    --bs-emphasis-color-rgb: 255,255,255
    LIGHT THEME:
    --bs-emphasis-color: #000
    --bs-emphasis-color-rgb: 0,0,0

  - 'border': For component borders, dividers, and rules.
    DARK THEME:
    --bs-border-color-translucent: rgba(255, 255, 255, 0.15)
    --bs-border-color: #495057
    LIGHT THEME:
    --bs-border-color-translucent: rgba(0, 0, 0, 0.175)
    --bs-border-color: #dee2e6

  - 'primary': Main theme color, used for hyperlinks, focus styles, and
  component and form active states.
    --bs-primary: #0d6efd
    --bs-primary-rgb: 13,110,253
    DARK THEME:
    --bs-primary-bg-subtle: #031633
    --bs-primary-border-subtle: #084298
    --bs-primary-text-emphasis: #6ea8fe
    LIGHT THEME:
    --bs-primary-bg-subtle: #cfe2ff
    --bs-primary-border-subtle: #9ec5fe
    --bs-primary-text-emphasis: #052c65

  - 'success': Theme color used for positive or successful actions and
  information.
    --bs-success: #198754
    --bs-success-rgb: 25,135,84
    DARK THEME:
    --bs-success-bg-subtle: #051b11
    --bs-success-border-subtle: #0f5132
    --bs-success-text-emphasis: #75b798
    LIGHT THEME:
    --bs-success-bg-subtle: #d1e7dd
    --bs-success-border-subtle: #a3cfbb
    --bs-success-text-emphasis: #0a3622

  - 'danger': Theme color used for errors and dangerous actions.
    --bs-danger: #dc3545
    --bs-danger-rgb: 220,53,69
    DARK THEME:
    --bs-danger-bg-subtle: #2c0b0e
    --bs-danger-border-subtle: #842029
    --bs-danger-text-emphasis: #ea868f
    LIGHT THEME:
    --bs-danger-bg-subtle: #f8d7da
    --bs-danger-border-subtle: #f1aeb5
    --bs-danger-text-emphasis: #58151c

  - 'warning': Theme color used for non-destructive warning messages.
    --bs-warning: #ffc107
    --bs-warning-rgb: 255,193,7
    DARK THEME:
    --bs-warning-bg-subtle: #332701
    --bs-warning-border-subtle: #997404
    --bs-warning-text-emphasis: #ffda6a
    LIGHT THEME:
    --bs-warning-bg-subtle: #fff3cd
    --bs-warning-border-subtle: #ffe69c
    --bs-warning-text-emphasis: #664d03

  - 'info': Theme color used for neutral and informative content.
    --bs-info: #0dcaf0
    --bs-info-rgb: 13,202,240
    DARK THEME:
    --bs-info-bg-subtle: #032830
    --bs-info-border-subtle: #087990
    --bs-info-text-emphasis: #6edff6
    LIGHT THEME:
    --bs-info-bg-subtle: #cff4fc
    --bs-info-border-subtle: #9eeaf9
    --bs-info-text-emphasis: #055160

  - 'light': Additional theme option for less contrasting colors.
    --bs-light: #f8f9fa
    --bs-light-rgb: 248,249,250
    DARK THEME:
    --bs-light-bg-subtle: #343a40
    --bs-light-border-subtle: #495057
    --bs-light-text-emphasis: #f8f9fa
    LIGHT THEME:
    --bs-light-bg-subtle: #fcfcfd
    --bs-light-border-subtle: #e9ecef
    --bs-light-text-emphasis: #495057

  - 'dark': Additional theme option for higher contrasting colors.
    --bs-dark: #212529
    --bs-dark-rgb: 33,37,41
    DARK THEME:
    --bs-dark-bg-subtle: #1a1d20
    --bs-dark-border-subtle: #343a40
    --bs-dark-text-emphasis: #dee2e6
    LIGHT THEME:
    --bs-dark-bg-subtle: #ced4da
    --bs-dark-border-subtle: #adb5bd
    --bs-dark-text-emphasis: #495057
  ")

(defparameter *breakpoints*
  '("xs" "sm" "md" "lg" "xl" "xxl")
  "List of breakpoint sizes defined in the Bootstrap framework.

  These correspond to various device screen sizes:

  - 'xs': Extra small screens (portrait phones)
  - 'sm': Small screens (landscape phones ≥576px)
  - 'md': Medium screens (tablets ≥768px)
  - 'lg': Large screens (small laptops and desktops ≥992px)
  - 'xl': Extra-large screens (large laptops and desktops ≥1200px)
  - 'xxl': Extremely large screens (full-size monitors ≥1400px)")

(defparameter *sides*
  '("t" "b" "s" "e" "x" "y")
  "List of sides for Bootstrap's margin and padding spacing utilities.

  Each character represents a side where the spacing utility can be applied:

  - 't': Top
  - 'b': Bottom
  - 's': Start (usually maps to 'left' in LTR layouts)
  - 'e': End (usually maps to 'right' in LTR layouts)
  - 'x': Horizontal axis (both 'Start' and 'End')
  - 'y': Vertical axis (both 'Top' and 'Bottom')")

(defun string-clean (str)
  (string-trim " " (string-downcase str)))

(defun background (&key (color "primary") (gradient nil))
  "Generates a Bootstrap background class.

COLOR: Specifies the color, should be 'primary', 'secondary', 'success',
'danger', 'warning', 'info', 'light', 'dark', 'body', 'muted', 'white', 'transparent',
or nil (default color).

GRADIENT: Specifies if the background should have a gradient, should be t or
nil. If t, it will add a 'bg-gradient' to the class string.

Example 1:
  (background :color \"primary\")
  ; This will generate a string 'bg-primary'

Example 2:
  (background :color \"danger\" :gradient t)
  ; This will generate a string 'bg-danger bg-gradient'

Example 3:
  (background :color \"light\")
  ; This will generate a string 'bg-light'

Example 4:
  (background :color \"dark\" :gradient t)
  ; This will generate a string 'bg-dark bg-gradient'"
  (assert (member color *colors* :test #'string=) nil "Color can't be nil")
  (let ((color-str (format nil "bg-~a" color))
        (gradient-str (if (null gradient) "" " bg-gradient")))
    (string-clean (concatenate 'string color-str gradient-str))))

(defun color (&key (text nil) (background nil) (emphasis nil) (body nil))
  "Generates a Bootstrap color class.

TEXT: Specifies the color of the text. Should be 'primary', 'secondary',
'success', 'danger', 'warning', 'info', 'light', 'dark', 'white', 'body',
'muted', 'reset'

BACKGROUND: Specifies the color of the background. Should be 'primary',
'secondary', 'success', 'danger', 'warning', 'info', 'light', 'dark', 'white',
'transparent'.

EMPHASIS: Specifies whether the color should be emphasized or not.

BODY: Specifies whether the color of the text should be body color.

Example 1:
  (color :text \"primary\")
  ; This will generate a string 'text-primary'

Example 2:
  (color :background '(:color \"danger\"))
  ; This will generate a string 'bg-danger'

Example 3:
  (color :text \"info\" :background '(:color \"dark\"))
  ; This will generate a string 'text-info bg-dark'

Example 4:
  (color :text :white \"background\" '(:color \"primary\"))
  ; This will generate a string 'text-white bg-primary'

Example 5:
  (color :text \"primary\" :emphasis t)
  ; This will generate a string 'text-primary-emphasis'

Example 6:
  (color :body \"secondary\")
  ; This will generate a string 'text-body-secondary'

Example 7:
  (color :body t :emphasis t)
  ; This will generate a string 'text-body-emphasis'"
  (let* ((text-str (if (null text) "" (format nil "text-~a" text)))
         (background-str (if (null background) "" (apply #'background background)))
         (emphasis-str (if (null emphasis) "" "-emphasis"))
         (body-str (if (null body) "" (if (stringp body)
                                          (format nil "text-body-~a" body)
                                          "text-body"))))
    (string-clean (concatenate 'string
                               body-str
                               text-str
                               emphasis-str
                               (when background " ")
                               background-str))))

(defun opacity (&key (level nil))
  "Generates a Bootstrap opacity class.

LEVEL: Specifies the opacity level, should be a number from 0 to 100, or 'auto'.

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
  (opacity :level \"auto\")
  ; This will generate a string 'opacity-auto'"
  (assert (or (equal level "auto")
              (>= level 0)) nil "Level should be 'auto' or positive number")
  (let ((level-str (if (equal level "auto")
                       "opacity-auto"
                       (format nil "opacity-~d" level))))
    (string-clean (concatenate 'string level-str))))

(defun overflow (&key (direction nil) (value nil))
  "Generates a Bootstrap overflow class.

DIRECTION: Specifies the direction, should be 'x', 'y', or nil (both directions).

VALUE: Specifies the overflow value, should be 'auto', 'hidden', 'visible', 'scroll'.

Example 1:
  (overflow :direction \"x\" :value \"auto\")
  ; This will generate a string 'overflow-x-auto'

Example 2:
  (overflow :direction \"y\" :value \"hidden\")
  ; This will generate a string 'overflow-y-hidden'

Example 3:
  (overflow :value \"visible\")
  ; This will generate a string 'overflow-visible'

Example 4:
  (overflow :direction \"x\" :value \"scroll\")
  ; This will generate a string 'overflow-x-scroll'"
  (assert (member value '("auto" "hidden" "visible" "scroll") :test #'string=)
          nil
          "Direction or value should be set as string")
  (let* ((dir-str (if (null direction)
                      ""
                      (format nil "overflow-~a" direction)))
         (value-str (if (string= dir-str "")
                        (format nil "overflow-~a" value)
                        (format nil "-~a" value))))
    (string-clean (concatenate 'string dir-str value-str))))

(defun sizing (&key (direction nil) (size nil))
  "Generates a Bootstrap sizing class.

DIRECTION: Specifies the direction, should be :w or :h.

SIZE: Specifies the size, should be a number from 0 to 100, :25, :50, :75,
:100, :auto, or nil (default size).

Example 1:
  (sizing :direction \"w\" :size 50)
  ; This will generate a string 'w-50'

Example 2:
  (sizing :direction \"h\" :size \"auto\")
  ; This will generate a string 'h-auto'

Example 3:
  (sizing :direction \"w\" :size 100)
  ; This will generate a string 'w-100'

Example 4:
  (sizing :direction \"h\" :size 75)
  ; This will generate a string 'h-75'"
  (assert (and (member direction '("w" "h") :test #'string=)
               (or (equal size "auto") (>= size 0)))
          nil "DIRECTION and SIZE can't be nil")
  (let* ((dir-str (format nil "~a-" direction))
         (size-str (if (equal size "auto")
                       "auto"
                       (if (numberp size)
                           (format nil "~d" size)
                           (format nil "~a" size)))))
    (string-clean (concatenate 'string dir-str size-str))))

(defun spacing (&key (property nil) (side nil) (size nil) (breakpoint nil))
  "Generates a Bootstrap spacing class.

PROPERTY: Specifies the property, should be 'm' (margin) or 'p' (padding).

SIDE: Specifies the side, should be 't' (top), 'b' (bottom), 's' (start),
'e' (end), 'x' (horizontal), 'y' (vertical), or nil (all sides).

SIZE: Specifies the size, should be a number from 0 to 5, or 'auto'.

BREAKPOINT: Specifies the breakpoint, should be 'xs', 'sm', 'md', 'lg', 'xl',
or 'xxl', or nil (all breakpoints).

Example 1:
  (spacing :property \"m\" :side \"t\" :size 3 :breakpoint \"md\")
  ; This will generate a string 'mt-md-3'

Example 2:
  (spacing :property \"p\" :side \"b\" :size 2 :breakpoint \"lg\")
  ; This will generate a string 'pb-lg-2', which represents a large breakpoint
  ; with bottom padding of size 2.

Example 3:
  (spacing :property \"m\" :size \"auto\")
  ; This will generate a string 'm-auto', which sets auto margin on all sides
  ; for all breakpoints.

Example 4:
  (spacing :property \"p\" :side \"x\" :size 5)
  ; This will generate a string 'px-5', which sets horizontal padding of size 5
  ; for all breakpoints."
  (assert (and property size) nil "Property and Size needed")
  (assert (member property '("m" "p") :test #'string=)
          nil "Property should be 'm' or 'p'")
  (assert (or (and (numberp size) (>= size 0))
              (equal size "auto"))
          nil "Size should be greater than or equal to 0 or 'auto'")
  (when side (assert (member side *sides* :test #'string=)))
  (when breakpoint (assert (member breakpoint *breakpoints* :test #'string=)
                           nil "Breakpoint should be 'xs', 'sm', 'md', 'lg', 'xl', 'xxl'"))

  (let ((side-str (if (null side) "" side))
        (size-str (if (equal size "auto") "-auto" (format nil "-~d" size)))
        (breakpoint-str (if (null breakpoint) "" (format nil "-~a" breakpoint))))
    (string-clean
     (concatenate 'string
                  property
                  side-str
                  breakpoint-str
                  size-str))))

(defun text (&key (alignment nil) (transform nil) (weight nil) (wrap nil) (monospace nil))
  "Generates a Bootstrap text utility class.

ALIGNMENT: Specifies the text alignment. Should be 'start', 'end', 'center'.

TRANSFORM: Specifies the text transformation. Should be 'lowercase',
'uppercase', 'capitalize'.

WEIGHT: Specifies the text weight. Should be 'bold', 'bolder', 'normal',
'light', 'lighter'.

WRAP: Specifies the text wrapping option. Should be 'wrap' or 'nowrap'.

MONOSPACE: If true, sets the font to monospace.

Example 1:
  (text :alignment \"start\")
  ; Generates a string 'text-start'

Example 2:
  (text :transform \"uppercase\")
  ; Generates a string 'text-uppercase'

Example 3:
  (text :weight \"bold\" :monospace t)
  ; Generates a string 'fw-bold font-monospace'

Example 4:
  (text :alignment \"center\" :transform \"lowercase\")
  ; Generates a string 'text-center text-lowercase'

Example 5:
  (text :alignment \"end\" :weight \"light\" :monospace t)
  ; Generates a string 'text-end fw-light font-monospace '

Example 6:
  (text :transform \"capitalize\" :wrap \"wrap\")
  ; Generates a string 'text-capitalize text-wrap '

Example 7:
  (text :alignment \"center\" :transform \"uppercase\" :weight \"bolder\" :wrap \"nowrap\" :monospace t)
  ; Generates a string 'text-center text-uppercase fw-bolder
  ; text-nowrap font-monospace '"
  (assert (or alignment transform weight wrap monospace)
          nil "Provide at least one argument")
  (when alignment
    (assert (member alignment '("start" "end" "center")
                    :test #'string=)
            nil "ALIGNMENT should be of 'start', 'end' or 'center'"))
  (when transform
    (assert (member transform '("lowercase" "uppercase" "capitalize")
                    :test #'string=)
            nil "TRANSFORM should be of 'lowercase', 'uppercase' 'capitalize'"))
  (when weight
    (assert (member weight '("bold" "bolder" "normal" "light" "lighter")
                    :test #'string=)
            nil "WEIGHT should be of 'bold', 'bolder', 'normal', 'light', 'lighter'"))
  (when wrap
    (assert (member wrap '("wrap" "nowrap") :test #'string=)
            nil "WRAP should be of 'wrap' or 'nowrap'"))
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

(defun valign (align)
  "Generates a Bootstrap vertical align class.

ALIGN: Specifies the alignment, should be 'baseline', 'top', 'middle', 'bottom',
'text-bottom', 'text-top'.

Example 1:
  (valign \"baseline\")
  ; This will generate a string 'align-baseline'

Example 2:
  (valign \"top\")
  ; This will generate a string 'align-top'

Example 3:
  (valign \"middle\")
  ; This will generate a string 'align-middle'"
  (let ((align-str (if (null align) "" (format nil "align-~a" align))))
    (string-clean align-str)))
