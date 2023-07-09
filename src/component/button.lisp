;; https://getbootstrap.com/docs/5.3/components/buttons/

;; Use Bootstrap’s custom button styles for actions in forms, dialogs, and more
;; with support for multiple sizes, states, and more.

;; Bootstrap has a base .btn class that sets up basic styles such as padding and
;; content alignment. By default, .btn controls have a transparent border and
;; background color, and lack any explicit focus and hover styles.

(in-package :cl-sbt)

(defmacro btn ((&key (type nil) (size nil)) &body body)
  `(spinneret:with-html
     (:button :type "button"
              :class (string-downcase (concatenate 'string
                                                   "btn"
                                                   (if (null ,type) nil (format nil " btn-~a" ,type))
                                                   (if (null ,size) nil (format nil " btn-~a" ,size))))
              ,@body)))

(defmacro define-btns (names)
  `(progn
     ,@(loop for item in names
             for symbol = (intern (concatenate 'string "BTN-" (symbol-name item)))
             for item-name = (format nil "~a" item)
             collect `(defmacro ,symbol (&body body)
                        `(btn (:type ,(string ',item-name)) ,@body)))))

(define-btns (primary secondary success danger warning info light dark link))

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
