;; https://getbootstrap.com/docs/5.3/components/alerts/

;; The Bootstrap Alert component is a flexible, dismissible container for
;; displaying important, often short, messages to the user. It's primarily used
;; for system messaging.

;; The Alerts are available for various types and states. They can be of different
;; types such as success, info, warning, danger, primary, secondary, light, and
;; dark to indicate the nature of the alert. Each type is visually distinguished
;; by a different color.

;; The content inside an alert can be nearly any valid HTML, including headings,
;; paragraphs, and links.

;; By adding an optional .alert-dismissible class to the alert container, and a
;; close button, an alert can be made dismissible - it will disappear when the
;; user clicks the close button. This is especially useful for ephemeral messages
;; like form validation errors, where the message does not need to persist once
;; the user has seen it.

;; Here is a basic example of a Bootstrap Alert component:
;; <div class="alert alert-warning alert-dismissible fade show" role="alert">
;;   <strong>Warning!</strong> You should check in on some of those fields below.
;;   <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
;; </div>

(defpackage cl-sbt-alert
  (:use :cl)
  (:export
   :btn
   :alert))

(in-package :cl-sbt-alert)

(defmacro btn ()
  "This macro generates the close button used in a Bootstrap alert when it is set to be dismissible."

  `(spinneret:with-html
     (:button :type "button"
              :class "btn-close"
              :data-bs-dismiss "alert"
              :aria-label "Close")))

(defmacro alert ((&key (type "primary") (dismissible nil)) &body body)
  "This macro generates a Bootstrap alert component.

   Parameters:
   - TYPE: Specifies the alert type. Can be 'primary', 'secondary', 'success', 'danger', 'warning', 'info', 'light', or 'dark'. Defaults to 'primary'.
   - DISMISSIBLE: Specifies whether the alert is dismissible. If true, the alert includes a close button.
   - BODY: Specifies the content of the alert."

  `(spinneret:with-html
     (:div :role "alert"
           :class ,(concatenate 'string (format nil "alert alert-~a" type)
                               (if (null dismissible) nil " alert-dismissible"))
           ,(if (null dismissible) nil `(btn))
           ,@body)))

;; (defmacro alert-primary (&body body)
;;   `(alert (:type "primary") ,@body))

;; (defmacro alert-secondary (&body body)
;;   `(alert (:type "secondary") ,@body))

;; (defmacro alert-success (&body body)
;;   `(alert (:type "success") ,@body))

;; (defmacro alert-danger (&body body)
;;   `(alert (:type "danger") ,@body))

;; (defmacro alert-warning (&body body)
;;   `(alert (:type "warning") ,@body))

;; (defmacro alert-info (&body body)
;;   `(alert (:type "info") ,@body))

;; (defmacro alert-light (&body body)
;;   `(alert (:type "light") ,@body))

;; (defmacro alert-dark (&body body)
;;   `(alert (:type "dark") ,@body))

;; (defmacro alert-primary-dismiss (&body body)
;;   `(alert (:type "primary" :dismissible t) ,@body))

;; (defmacro alert-secondary-dismiss (&body body)
;;   `(alert (:type "secondary" :dismissible t) ,@body))

;; (defmacro alert-success-dismiss (&body body)
;;   `(alert (:type "success" :dismissible t) ,@body))

;; (defmacro alert-danger-dismiss (&body body)
;;   `(alert (:type "danger" :dismissible t) ,@body))

;; (defmacro alert-warning-dismiss (&body body)
;;   `(alert (:type "warning" :dismissible t) ,@body))

;; (defmacro alert-info-dismiss (&body body)
;;   `(alert (:type "info" :dismissible t) ,@body))

;; (defmacro alert-light-dismiss (&body body)
;;   `(alert (:type "light" :dismissible t) ,@body))

;; (defmacro alert-dark-dismiss (&body body)
;;   `(alert (:type "dark" :dismissible t) ,@body))
