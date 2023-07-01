(in-package :cl-sbt)

(defmacro btn ((&key (type nil) (size nil)) &body body)
  `(spinneret:with-html
     (:button :type "button"
              :class (concatenate 'string "btn"
                                  (if ,type (format nil " btn-~a" ,type))
                                  (if ,size (format nil " btn-~a" ,size)))
              ,@body)))

(defmacro btn-primary (&body body)
  `(btn (:type "primary") ,@body))

(defmacro btn-secondary (&body body)
  `(btn (:type "secondary") ,@body))

(defmacro btn-success (&body body)
  `(btn (:type "success") ,@body))

(defmacro btn-danger (&body body)
  `(btn (:type "danger") ,@body))

(defmacro btn-warning (&body body)
  `(btn (:type "warning") ,@body))

(defmacro btn-info (&body body)
  `(btn (:type "info") ,@body))

(defmacro btn-light (&body body)
  `(btn (:type "light") ,@body))

(defmacro btn-dark (&body body)
  `(btn (:type "dark") ,@body))

(defmacro btn-link (&body body)
  `(btn (:type "link") ,@body))

(defmacro btn-outline-primary (&body body)
  `(btn (:type "outline-primary") ,@body))

(defmacro btn-outline-secondary (&body body)
  `(btn (:type "outline-secondary") ,@body))

(defmacro btn-outline-success (&body body)
  `(btn (:type "outline-success") ,@body))

(defmacro btn-outline-danger (&body body)
  `(btn (:type "outline-danger") ,@body))

(defmacro btn-outline-warning (&body body)
  `(btn (:type "outline-warning") ,@body))

(defmacro btn-outline-info (&body body)
  `(btn (:type "outline-info") ,@body))

(defmacro btn-outline-light (&body body)
  `(btn (:type "outline-light") ,@body))

(defmacro btn-outline-dark (&body body)
  `(btn (:type "outline-dark") ,@body))

(defmacro btn-outline-link (&body body)
  `(btn (:type "outline-link") ,@body))
