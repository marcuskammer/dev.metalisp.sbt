;; https://getbootstrap.com/docs/5.3/components/buttons/

;; Use Bootstrap’s custom button styles for actions in forms, dialogs, and more
;; with support for multiple sizes, states, and more.

;; Bootstrap has a base .btn class that sets up basic styles such as padding and
;; content alignment. By default, .btn controls have a transparent border and
;; background color, and lack any explicit focus and hover styles.

(defpackage cl-sbt-btn
  (:use :cl)
  (:export
   :btn
   :btn-primary
   :btn-secondary
   :btn-success
   :btn-danger
   :btn-warning
   :btn-info
   :btn-light
   :btn-dark
   :btn-link
   :btn-primary-lg
   :btn-secondary-lg
   :btn-success-lg
   :btn-danger-lg
   :btn-warning-lg
   :btn-info-lg
   :btn-light-lg
   :btn-dark-lg
   :btn-link-lg
   :btn-primary-sm
   :btn-secondary-sm
   :btn-success-sm
   :btn-danger-sm
   :btn-warning-sm
   :btn-info-sm
   :btn-light-sm
   :btn-dark-sm
   :btn-link-sm))

(in-package :cl-sbt-btn)

(defmacro btn ((&key (type nil) (size nil)) &body body)
  "This macro generates a Bootstrap button.

   TYPE: (optional) The type of the button (like 'primary', 'secondary', 'success', etc.).
   SIZE: (optional) The size of the button ('lg' for large, 'sm' for small).
   BODY: The contents of the button."

  `(spinneret:with-html
     (:button :type "button"
              :class  (concatenate 'string
                                   "btn"
                                   (if (null ,type) nil (format nil " btn-~a" ,type))
                                   (if (null ,size) nil (format nil " btn-~a" ,size)))
              ,@body)))

(defmacro define-btns (names)
  "This macro generates specific button macros based on the provided names.

   NAMES: A list of button type names. For each name in this list, three macros will be generated: a standard button, a large button, and a small button."

  `(progn
     ,@(loop for item in names
             for symbol = (intern (concatenate 'string "BTN-" (symbol-name item)))
             for symbol-lg = (intern (concatenate 'string "BTN-" (symbol-name item) "-LG"))
             for symbol-sm = (intern (concatenate 'string "BTN-" (symbol-name item) "-SM"))
             for item-name = (string-downcase (format nil "~a" item))
             collect `(progn
                        (defmacro ,symbol (&body body)
                          `(btn (:type ,(string ',item-name)) ,@body))
                        (defmacro ,symbol-lg (&body body)
                          `(btn (:type ,(string ',item-name) :size "lg") ,@body))
                        (defmacro ,symbol-sm (&body body)
                          `(btn (:type ,(string ',item-name) :size "sm") ,@body))))))

(define-btns (primary secondary success danger warning info light dark link))

;; (defmacro btn-primary (&body body)
;;   `(btn (:type "primary") ,@body))

;; (defmacro btn-secondary (&body body)
;;   `(btn (:type "secondary") ,@body))

;; (defmacro btn-success (&body body)
;;   `(btn (:type "success") ,@body))

;; (defmacro btn-danger (&body body)
;;   `(btn (:type "danger") ,@body))

;; (defmacro btn-warning (&body body)
;;   `(btn (:type "warning") ,@body))

;; (defmacro btn-info (&body body)
;;   `(btn (:type "info") ,@body))

;; (defmacro btn-light (&body body)
;;   `(btn (:type "light") ,@body))

;; (defmacro btn-dark (&body body)
;;   `(btn (:type "dark") ,@body))

;; (defmacro btn-link (&body body)
;;   `(btn (:type "link") ,@body))

;; (defmacro btn-primary-lg (&body body)
;;   `(btn (:type "primary" :size "lg") ,@body))

;; (defmacro btn-secondary-lg (&body body)
;;   `(btn (:type "secondary" :size "lg") ,@body))

;; (defmacro btn-success-lg (&body body)
;;   `(btn (:type "success" :size "lg") ,@body))

;; (defmacro btn-danger-lg (&body body)
;;   `(btn (:type "danger" :size "lg") ,@body))

;; (defmacro btn-warning-lg (&body body)
;;   `(btn (:type "warning" :size "lg") ,@body))

;; (defmacro btn-info-lg (&body body)
;;   `(btn (:type "info" :size "lg") ,@body))

;; (defmacro btn-light-lg (&body body)
;;   `(btn (:type "light" :size "lg") ,@body))

;; (defmacro btn-dark-lg (&body body)
;;   `(btn (:type "dark" :size "lg") ,@body))

;; (defmacro btn-link-lg (&body body)
;;   `(btn (:type "link" :size "lg") ,@body))

;; (defmacro btn-outline-primary (&body body)
;;   `(btn (:type "outline-primary") ,@body))

;; (defmacro btn-outline-secondary (&body body)
;;   `(btn (:type "outline-secondary") ,@body))

;; (defmacro btn-outline-success (&body body)
;;   `(btn (:type "outline-success") ,@body))

;; (defmacro btn-outline-danger (&body body)
;;   `(btn (:type "outline-danger") ,@body))

;; (defmacro btn-outline-warning (&body body)
;;   `(btn (:type "outline-warning") ,@body))

;; (defmacro btn-outline-info (&body body)
;;   `(btn (:type "outline-info") ,@body))

;; (defmacro btn-outline-light (&body body)
;;   `(btn (:type "outline-light") ,@body))

;; (defmacro btn-outline-dark (&body body)
;;   `(btn (:type "outline-dark") ,@body))

;; (defmacro btn-outline-link (&body body)
;;   `(btn (:type "outline-link") ,@body))
