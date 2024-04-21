;;;; badge.lisp
;;;;
;;;; This file constitutes the Common Lisp package for generating Bootstrap badges,
;;;; a lightweight and versatile component used to add contextual highlights or emphasis.

(defpackage dev.metalisp.sbt/component/badge
  (:documentation "A package for generating Bootstrap badges.")
  (:use :cl)
  (:export
   :badge
   :badge-primary
   :badge-secondary
   :badge-success
   :badge-danger
   :badge-warning
   :badge-info
   :badge-light
   :badge-dark
   :badge-link
   :badge-pill-primary
   :badge-pill-secondary
   :badge-pill-success
   :badge-pill-danger
   :badge-pill-warning
   :badge-pill-info
   :badge-pill-light
   :badge-pill-dark
   :badge-pill-link))

(in-package :dev.metalisp.sbt/component/badge)

(defmacro badge ((&key (color "primary") (pill nil)) &body body)
  "This macro generates a Bootstrap badge.

COLOR: The type of the badge (like 'primary', 'secondary', 'success', etc.). Defaults to 'primary'.

BODY: The contents of the badge.

Example:
  (badge (:color \"success\" :pill t) \"New\")"
  (let ((class-str (concatenate 'string
                                (format nil "badge text-bg-~a" color)
                                (if (null pill) "" " rounded-pill"))))
    `(spinneret:with-html
       (:span :class ,class-str
              ,@body))))

(defmacro define-badge (color &optional (pill nil))
  "This macro defines a new macro for creating a Bootstrap badge of a specific type.

COLOR: The color of the badge (like 'primary', 'secondary', 'success', etc.).

PILL: (optional) If true, the badge will have 'rounded-pill' style.

The newly defined macro, when called, will generate HTML for a Bootstrap
badge of the specified type."
  (let ((macro-name (intern (string-upcase (concatenate 'string "BADGE-" (if (null pill) "" "PILL-") color)))))
    `(defmacro ,macro-name (&body body)
       `(badge (:color ,,color :pill ,,pill) ,@body))))

(defmacro define-badges (colors)
  "This macro generates specific badge macros based on the provided names.

COLORS: A list of badge color names. For each name in this list, a macro will
be generated: a badge of the specified type."
  `(progn
     ,@(loop for color in colors
             for color-name = (string-downcase (string color))
             collect `(progn
                        (define-badge ,color-name)
                        (define-badge ,color-name t)))))

(define-badges (primary secondary success danger warning info light dark link))
