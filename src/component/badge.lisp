;; https://getbootstrap.com/docs/5.3/components/badge/

(in-package :cl-sbt)

(defmacro badge ((&key (classes nil)) &body body)
  "https://getbootstrap.com/docs/5.3/components/badge/
Documentation and examples for badges, our small count and labeling component."
  `(spinneret:with-html
     (:span :class (concatenate 'string "badge " ,classes)
            ,@body)))

(defmacro badge-primary (&body body)
  `(badge (:classes "text-bg-primary") ,@body))

(defmacro badge-secondary (&body body)
  `(badge (:classes "text-bg-secondary") ,@body))

(defmacro badge-success (&body body)
  `(badge (:classes "text-bg-success") ,@body))

(defmacro badge-danger (&body body)
  `(badge (:classes "text-bg-danger") ,@body))

(defmacro badge-warning (&body body)
  `(badge (:classes "text-bg-warning") ,@body))

(defmacro badge-info (&body body)
  `(badge (:classes "text-bg-info") ,@body))

(defmacro badge-light (&body body)
  `(badge (:classes "text-bg-light") ,@body))

(defmacro badge-dark (&body body)
  `(badge (:classes "text-bg-dark") ,@body))

(defmacro badge-pill-primary (&body body)
  `(badge (:classes "rounded-pill text-bg-primary") ,@body))

(defmacro badge-pill-secondary (&body body)
  `(badge (:classes "rounded-pill text-bg-secondary") ,@body))

(defmacro badge-pill-success (&body body)
  `(badge (:classes "rounded-pill text-bg-success") ,@body))

(defmacro badge-pill-danger (&body body)
  `(badge (:classes "rounded-pill text-bg-danger") ,@body))

(defmacro badge-pill-warning (&body body)
  `(badge (:classes "rounded-pill text-bg-warning") ,@body))

(defmacro badge-pill-info (&body body)
  `(badge (:classes "rounded-pill text-bg-info") ,@body))

(defmacro badge-pill-light (&body body)
  `(badge (:classes "rounded-pill text-bg-light") ,@body))

(defmacro badge-pill-dark (&body body)
  `(badge (:classes "rounded-pill text-bg-dark") ,@body))
