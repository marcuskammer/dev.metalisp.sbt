;; https://getbootstrap.com/docs/5.3/components/alerts/

;; Provide contextual feedback messages for typical user actions with the handful
;; of available and flexible alert messages. Alerts are available for any length
;; of text, as well as an optional close button. For proper styling, use one of
;; the eight required contextual classes (e.g., .alert-success).

(in-package :cl-sbt)

(defmacro alert-btn ()
  `(spinneret:with-html
     (:button :type "button"
              :class "btn-close"
              :data-bs-dismiss "alert"
              :aria-label "Close")))

(defmacro alert ((&key (type nil) (dismissible nil)) &body body)
  `(spinneret:with-html
     (:div :role "alert"
           :class (concatenate 'string (format nil "alert alert-~a" ,type)
                                (when ,dismissible " alert-dismissible"))
           ,(when dismissible
              `(alert-btn))
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

(defmacro alert-primary-dismiss (&body body)
  `(alert (:type "primary" :dismissible t) ,@body))

(defmacro alert-secondary-dismiss (&body body)
  `(alert (:type "secondary" :dismissible t) ,@body))

(defmacro alert-success-dismiss (&body body)
  `(alert (:type "success" :dismissible t) ,@body))

(defmacro alert-danger-dismiss (&body body)
  `(alert (:type "danger" :dismissible t) ,@body))

(defmacro alert-warning-dismiss (&body body)
  `(alert (:type "warning" :dismissible t) ,@body))

(defmacro alert-info-dismiss (&body body)
  `(alert (:type "info" :dismissible t) ,@body))

(defmacro alert-light-dismiss (&body body)
  `(alert (:type "light" :dismissible t) ,@body))

(defmacro alert-dark-dismiss (&body body)
  `(alert (:type "dark" :dismissible t) ,@body))
