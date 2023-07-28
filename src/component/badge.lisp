;; https://getbootstrap.com/docs/5.3/components/badge/

;; The Bootstrap Badge component is a lightweight, flexible utility used to add
;; contextual emphasis or highlight to an item by placing a small pill-shaped
;; or circular badge on it. It is often used for items like notifications,
;; status, or tagging.

;; Badges are created using basic HTML with the class .badge, often in
;; conjunction with classes like .bg-primary, .bg-success, etc. to style the
;; color of the badge. Further customization can be achieved using Bootstrap's
;; extensive utility classes for things like sizing, position, and visibility.

;; Here are a few key features of the Bootstrap Badge component:

;; Contextual variations: Bootstrap provides badge classes for various purposes
;; like success, danger, warning, info, light, and dark.

;; Pill badges: Add .rounded-pill to make badges round in shape.

;; Links: Badges can optionally be made clickable by using them within <a> or
;; <button> elements.

;; Placement: Badges can be placed on almost any other Bootstrap component,
;; including buttons, navs, and cards.

;; Dismissable: With a little extra HTML, badges can also be made dismissable.

;; The Bootstrap Badge component is a simple yet powerful tool for adding
;; contextual information to your web pages. It's fully responsive, easy to
;; use, and customizable to fit your needs.

(defpackage cl-sbt/badge
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

(in-package :cl-sbt/badge)

(defmacro badge ((&key (type "primary") (pill nil)) &body body)
  "This macro generates a Bootstrap badge.

TYPE: (optional) The type of the badge (like 'primary', 'secondary', 'success', etc.). Defaults to 'primary'.

BODY: The contents of the badge.

Example usage:
(badge (:type \"success\" :pill t) \"New\")"
  `(spinneret:with-html
     (:span :class ,(concatenate 'string
                                 (format nil "badge text-bg-~a" type)
                                 (if (null pill) "" " rounded-pill"))
            ,@body)))

(defmacro define-badge (type &optional (pill nil))
  "This macro defines a new macro for creating a Bootstrap badge of a specific type.

TYPE: The type of the badge (like 'primary', 'secondary', 'success', etc.).

PILL: (optional) If true, the badge will have 'rounded-pill' style.

The newly defined macro, when called, will generate HTML for a Bootstrap
badge of the specified type."
  (let* ((macro-name (intern (string-upcase (concatenate 'string "BADGE-" (if (null pill) "" "PILL-") type)))))
    `(defmacro ,macro-name (&body body)
       `(badge (:type ,,type :pill ,,pill) ,@body))))

(defmacro define-badges (names)
  "This macro generates specific badge macros based on the provided names.

NAMES: A list of badge type names. For each name in this list, a macro will
be generated: a badge of the specified type."
  `(progn
     ,@(loop for item in names
             for type-name = (string-downcase (string item))
             collect `(progn
                        (define-badge ,type-name)
                        (define-badge ,type-name t)))))

(define-badges (primary secondary success danger warning info light dark link))
