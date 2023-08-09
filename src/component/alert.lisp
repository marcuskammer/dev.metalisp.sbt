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

(defpackage cl-sbt/alert
  (:use :cl)
  (:export
   :btn
   :alert
   :alert-primary
   :alert-secondary
   :alert-success
   :alert-danger
   :alert-warning
   :alert-info
   :alert-light
   :alert-dark
   :alert-dismiss-primary
   :alert-dismiss-secondary
   :alert-dismiss-success
   :alert-dismiss-danger
   :alert-dismiss-warning
   :alert-dismiss-info
   :alert-dismiss-light
   :alert-dismiss-dark))

(in-package :cl-sbt/alert)

(defmacro btn ()
  "This macro generates the close button used in a Bootstrap alert when it is set to be dismissible."

  `(spinneret:with-html
     (:button :type "button"
              :class "btn-close"
              :data-bs-dismiss "alert"
              :aria-label "Close")))

(defmacro alert ((&key (color "primary") (dismissible nil)) &body body)
  "This macro generates a Bootstrap alert component.

COLOR: Specifies the alert type. Can be 'primary', 'secondary', 'success', 'danger', 'warning', 'info', 'light', or 'dark'. Defaults to 'primary'.

DISMISSIBLE: Specifies whether the alert is dismissible. If true, the alert includes a close button.

BODY: Specifies the content of the alert.

Example usage:
To create a basic alert of type 'danger':
(alert (:role \"danger\") \"This is a dangerous alert. Be careful!\")

To create a dismissible alert of type 'success':
(alert (:type \"success\" :dismissible t) \"Congratulations! You've successfully created a dismissible alert.\")"
  `(spinneret:with-html
     (:div :role "alert"
           :class ,(concatenate 'string (format nil "alert alert-~a" color)
                               (if (null dismissible) nil " alert-dismissible"))
           ,(if (null dismissible) nil `(btn))
           ,@body)))

(defmacro define-alert (color &optional (dismissible nil))
  "This macro defines a new macro for creating a Bootstrap alert of a specific type.

COLOR: The color of the alert (like 'primary', 'secondary', 'success', etc.).

DISMISSIBLE: (optional) Whether the alert should be dismissible.

The newly defined macro, when called, will generate HTML for a Bootstrap
alert of the specified type and dismissibility."
  (let* ((macro-name (intern (string-upcase (concatenate 'string "ALERT-" (if (null dismissible) "" "DISMISS-") color)))))
    `(defmacro ,macro-name (&body body)
       `(alert (:color ,,color :dismissible ,,dismissible) ,@body))))

(defmacro define-alerts (colors)
  "This macro generates specific alert macros based on the provided names.

COLORS: A list of alert type names. For each name in this list, a macro will
be generated: a non-dismissible alert and a dismissible alert of the
specified type."
  `(progn
     ,@(loop for color in colors
             for color-name = (string-downcase (string color))
             collect `(progn
                        (define-alert ,color-name)
                        (define-alert ,color-name t)))))

(define-alerts (primary secondary success danger warning info light dark))
