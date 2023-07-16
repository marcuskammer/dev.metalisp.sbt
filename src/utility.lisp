(defpackage :cl-sbt-utility
  (:use :cl)
  (:export
   :spacing
   :text)
  (:documentation "A module for generating Bootstrap utility classes."))

(in-package :cl-sbt-utility)

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

(defun text (&rest args)
  "Generates a Bootstrap text class based on provided arguments.

ARGS: A list of keyword arguments specifying the properties of the text. For
example, (:alignment :center :transform :capitalize) would result in the string
'text-center text-capitalize'.

Returns a string that can be used as a CSS class for the text."
  (let ((alignment (cadr (member :alignment args)))
        (wrap (cadr (member :wrap args)))
        (tbreak (cadr (member :break args))))
    (concatenate 'string
                 (case alignment
                   (:start "text-start ")
                   (:center "text-center ")
                   (:end "text-end ")
                   (t "text-start "))
                 (case wrap
                   (:t "text-wrap")
                   (:no "text-nowrap")
                   (t ""))
                 (case tbreak
                   (:t "text-break")
                   (t "")))))