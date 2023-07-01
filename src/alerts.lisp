(in-package :cl-sbt)

(defmacro sbt-alert ((&key (type "success") (dismissible nil)) &body body)
  "Generic alert"
  `(spinneret:with-html
     (:div :class ,(if dismissible
                       (format nil "alert alert-~a alert-dismissible" type)
                       (format nil "alert alert-~a" type)) :role "alert"
                       ,(when dismissible
                          `(:button :type "button" :class "close" :data-dismiss "alert" :aria-label "Close"
                                    (:span :aria-hidden "true" "&times;")))
                       ,@body)))
