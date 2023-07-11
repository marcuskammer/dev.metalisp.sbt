;; https://getbootstrap.com/docs/5.3/components/badge/

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
