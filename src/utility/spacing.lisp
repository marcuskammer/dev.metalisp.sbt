;; Bootstrap's spacing utility is a powerful toolset that allows developers to
;; modify an element's margin and padding with a range of classes. These
;; classes are built using a consistent scale of spacing variables for
;; predictable spacing across the application, ensuring a unified look and feel
;; throughout.

;; The spacing classes follow a simple {property}{sides}-{size} pattern for
;; defining the space on the widget. 'Property' is either 'm' for margins or
;; 'p' for padding. 'Sides' refers to the side of the element where the space
;; should be applied: top (t), bottom (b), left (start/s), right (end/e), x
;; (horizontal sides), y (vertical sides), or blank (all sides). 'Size'
;; represents the size of the space, ranging from 0 (no space) to 5 (most
;; space), or 'auto' for automatic margins.

;; Furthermore, Bootstrap's spacing utility also incorporates responsive
;; variations, meaning developers can assign spacing values that are specific
;; to a certain breakpoint, ensuring the design's responsiveness. These classes
;; follow the {property}{sides}-{breakpoint}-{size} pattern and the breakpoint
;; can be 'sm', 'md', 'lg', 'xl', 'xxl'.

;; In summary, Bootstrap's spacing utility provides a flexible, responsive, and
;; coherent system for managing spaces in a web layout, aiding in creating
;; beautiful and well-structured user interfaces.

(defpackage :cl-sbt-spacing
  (:use :cl)
  (:export :spacing)
  (:documentation "A module for generating Bootstrap spacing classes."))

(in-package :cl-sbt-spacing)

(defun spacing (&key (property nil) (side nil) (size nil) (breakpoint nil))
  "Generates a Bootstrap spacing class.

PROPERTY: Specifies the property, should be :m (margin) or :p (padding).

SIDE: Specifies the side, should be :t (top), :b (bottom), :s (start),
:e (end), :x (horizontal), :y (vertical), or nil (all sides).

SIZE: Specifies the size, should be a number from 0 to 5, or :auto.

BREAKPOINT: Specifies the breakpoint, should be :xs, :sm, :md, :lg, :xl, or
:xxl, or nil (all breakpoints).

Example 1:
  (spacing (:property :m :side :t :size 3 :breakpoint :md))
  ; This will generate a string 'mt-md-3'

Example 2:
  (spacing (:property :p :side :b :size 2 :breakpoint :lg))
  ; This will generate a string 'pb-lg-2', which represents a large breakpoint
  ; with bottom padding of size 2.

Example 3:
  (spacing (:property :m :size :auto))
  ; This will generate a string 'm-auto', which sets auto margin on all sides
  ; for all breakpoints.

Example 4:
  (spacing (:property :p :side :x :size 5))
  ; This will generate a string 'px-5', which sets horizontal padding of size 5
  ; for all breakpoints."
  (let ((property-str (if (null property) "" (string property)))
        (side-str (if (null side) "" (string side)))
        (size-str (if (null size) "" (if (eq size :auto) "auto" (format nil "~d" size))))
        (breakpoint-str (if (null breakpoint) "" (format nil "~a-" (string breakpoint)))))
    (string-downcase (concatenate 'string " " property-str side-str "-" breakpoint-str size-str))))
