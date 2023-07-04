(in-package :cl-sbt)

(defmacro navbar-header-about (&body body)
  `(spinneret:with-html
     (:div :class "col-sm-8 col-md-7 py-4"
           (:h4 "About")
           (:p :class "text-body-secondary"
               ,@body))))

(defmacro navbar-header-contact (&body body)
  `(spinneret:with-html
     (:div :class "col-sm-4 offset-md-1 py-4"
           (:h4 "Contact")
           (:ul :class "list-unstyled"
                ,@body))))

(defmacro navbar-header (&body body)
  `(spinneret:with-html
     (:div :class "text-bg-dark collapse"
           :id "navbarHeader"
           (:div :class "container"
                 (:div :class "row"
                       ,@body)))))

(defmacro navbar-brand ((&key (brand "Brand") (href "#")) &body body)
  `(spinneret:with-html
       (:a :class "navbar-brand d-flex align-items-center"
           :href ,href
           (:strong ,brand)
           ,@body)))

(defmacro navbar-toggler ((&key (target "#navbarHeader")) &body body)
  `(spinneret:with-html
       (:button :class "navbar-toggler collapsed"
                :type "button"
                :data-bs-toggle "collapse"
                :data-bs-target ,target
                :aria-controls "navbarHeader"
                :aria-expanded "false"
                :aria-label "Toggle navigation"
                (:span :class "navbar-toggler-icon")
                ,@body)))

(defmacro navbar (&body body)
  `(spinneret:with-html
       (:div :class "navbar"
             (:div :class "container"
                   ,@body))))

(defun show-navbar-header-contact (contacts)
  (navbar-header-contact (dolist (contact contacts)
                              (:li (:a :href (rest contact)
                                       :class "text-white"
                                       (first contact))))))

(defun show-navbar-header (about contacts)
  (navbar-header (navbar-header-about about)
    (show-navbar-header-contact contacts)))

(defun show-navbar (brand href target)
  (navbar () (navbar-brand (:brand brand :href href))
    (navbar-toggler (:target target))))
