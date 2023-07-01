(in-package :cl-sbt)

(defmacro alert ((&key (type nil) (dismissible nil)) &body body)
  `(spinneret:with-html
       (:div :class ,(if dismissible
                         (format nil "alert alert-~a alert-dismissible" type)
                         (format nil "alert alert-~a" type)) :role "alert"
                         ,(when dismissible
                            `(:button :type "button" :class "close" :data-dismiss "alert" :aria-label "Close"
                                      (:span :aria-hidden "true" "&times;")))
                         ,@body)))

(defmacro alert-primary (&body body)
  `(alert (:type "primary") ,@body))

(defmacro alert-secondary (&body body)
  `(alert (:type "secondary") ,@body))

(defmacro alert-success (&body body)
  `(alert (:type "success") ,@body))

(defmacro alert-danger (&body body)
  `(alert (:type "danger") ,@body))

(defmacro alert-warning (&body body)
  `(alert (:type "warning") ,@body))

(defmacro alert-info (&body body)
  `(alert (:type "info") ,@body))

(defmacro alert-light (&body body)
  `(alert (:type "light") ,@body))

(defmacro alert-dark (&body body)
  `(alert (:type "dark") ,@body))
