(in-package :cl-sbt)

(defmacro sbt-alert ((&key (type nil) (dismissible nil)) &body body)
  `(spinneret:with-html
       (:div :class ,(if dismissible
                         (format nil "alert alert-~a alert-dismissible" type)
                         (format nil "alert alert-~a" type)) :role "alert"
                         ,(when dismissible
                            `(:button :type "button" :class "close" :data-dismiss "alert" :aria-label "Close"
                                      (:span :aria-hidden "true" "&times;")))
                         ,@body)))

(defmacro sbt-alert-primary (&body body)
  `(sbt-alert (:type "primary") ,@body))

(defmacro sbt-alert-secondary (&body body)
  `(sbt-alert (:type "secondary") ,@body))

(defmacro sbt-alert-success (&body body)
  `(sbt-alert (:type "success") ,@body))

(defmacro sbt-alert-danger (&body body)
  `(sbt-alert (:type "danger") ,@body))

(defmacro sbt-alert-warning (&body body)
  `(sbt-alert (:type "warning") ,@body))

(defmacro sbt-alert-info (&body body)
  `(sbt-alert (:type "info") ,@body))

(defmacro sbt-alert-ligth (&body body)
  `(sbt-alert (:type "light") ,@body))

(defmacro sbt-alert-dark (&body body)
  `(sbt-alert (:type "dark") ,@body))
