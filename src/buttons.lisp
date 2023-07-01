(in-package :cl-sbt)

(defmacro sbt-btn ((&key (type nil) (size nil)) &body body)
  `(spinneret:with-html
     (:button :type "button"
              :class (concatenate 'string "btn"
                                  (if ,type (format nil " btn-~a" ,type))
                                  (if ,size (format nil " btn-~a" ,size)))
              ,@body)))

(defmacro sbt-btn-primary (&body body)
  `(sbt-btn (:type "primary") ,@body))

(defmacro sbt-btn-secondary (&body body)
  `(sbt-btn (:type "secondary") ,@body))

(defmacro sbt-btn-success (&body body)
  `(sbt-btn (:type "success") ,@body))

(defmacro sbt-btn-danger (&body body)
  `(sbt-btn (:type "danger") ,@body))

(defmacro sbt-btn-warning (&body body)
  `(sbt-btn (:type "warning") ,@body))

(defmacro sbt-btn-info (&body body)
  `(sbt-btn (:type "info") ,@body))

(defmacro sbt-btn-light (&body body)
  `(sbt-btn (:type "light") ,@body))

(defmacro sbt-btn-dark (&body body)
  `(sbt-btn (:type "dark") ,@body))

(defmacro sbt-btn-link (&body body)
  `(sbt-btn (:type "link") ,@body))

(defmacro sbt-btn-outline-primary (&body body)
  `(sbt-btn (:type "outline-primary") ,@body))

(defmacro sbt-btn-outline-secondary (&body body)
  `(sbt-btn (:type "outline-secondary") ,@body))

(defmacro sbt-btn-outline-success (&body body)
  `(sbt-btn (:type "outline-success") ,@body))

(defmacro sbt-btn-outline-danger (&body body)
  `(sbt-btn (:type "outline-danger") ,@body))

(defmacro sbt-btn-outline-warning (&body body)
  `(sbt-btn (:type "outline-warning") ,@body))

(defmacro sbt-btn-outline-info (&body body)
  `(sbt-btn (:type "outline-info") ,@body))

(defmacro sbt-btn-outline-light (&body body)
  `(sbt-btn (:type "outline-light") ,@body))

(defmacro sbt-btn-outline-dark (&body body)
  `(sbt-btn (:type "outline-dark") ,@body))

(defmacro sbt-btn-outline-link (&body body)
  `(sbt-btn (:type "outline-link") ,@body))
