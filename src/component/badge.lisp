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

(defpackage cl-sbt-badge
  (:use :cl)
  (:export
   :badge))

(in-package :cl-sbt-badge)

(defmacro badge ((&key (type "primary") (classes "")) &body body)
  "This macro generates a Bootstrap badge.

   Parameters:
   - TYPE: (optional) The type of the badge (like 'primary', 'secondary', 'success', etc.). Defaults to 'primary'.
   - CLASSES: (optional) Any additional CSS classes that should be added to the badge.
   - BODY: The contents of the badge."

  `(spinneret:with-html
     (:span :class (format nil "badge text-bg-~a ~a" ,type ,classes)
            ,@body)))

;; (defmacro badge-primary (&body body)
;;   `(badge (:classes "text-bg-primary") ,@body))

;; (defmacro badge-secondary (&body body)
;;   `(badge (:classes "text-bg-secondary") ,@body))

;; (defmacro badge-success (&body body)
;;   `(badge (:classes "text-bg-success") ,@body))

;; (defmacro badge-danger (&body body)
;;   `(badge (:classes "text-bg-danger") ,@body))

;; (defmacro badge-warning (&body body)
;;   `(badge (:classes "text-bg-warning") ,@body))

;; (defmacro badge-info (&body body)
;;   `(badge (:classes "text-bg-info") ,@body))

;; (defmacro badge-light (&body body)
;;   `(badge (:classes "text-bg-light") ,@body))

;; (defmacro badge-dark (&body body)
;;   `(badge (:classes "text-bg-dark") ,@body))

;; (defmacro badge-pill-primary (&body body)
;;   `(badge (:classes "rounded-pill text-bg-primary") ,@body))

;; (defmacro badge-pill-secondary (&body body)
;;   `(badge (:classes "rounded-pill text-bg-secondary") ,@body))

;; (defmacro badge-pill-success (&body body)
;;   `(badge (:classes "rounded-pill text-bg-success") ,@body))

;; (defmacro badge-pill-danger (&body body)
;;   `(badge (:classes "rounded-pill text-bg-danger") ,@body))

;; (defmacro badge-pill-warning (&body body)
;;   `(badge (:classes "rounded-pill text-bg-warning") ,@body))

;; (defmacro badge-pill-info (&body body)
;;   `(badge (:classes "rounded-pill text-bg-info") ,@body))

;; (defmacro badge-pill-light (&body body)
;;   `(badge (:classes "rounded-pill text-bg-light") ,@body))

;; (defmacro badge-pill-dark (&body body)
;;   `(badge (:classes "rounded-pill text-bg-dark") ,@body))
