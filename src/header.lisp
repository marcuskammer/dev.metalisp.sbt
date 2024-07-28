(in-package :ml-sbt)

(defvar header-contact-examples
  '(("Follow on Twitter" . "foo")
    ("Like on Facebook" . "foo")
    ("Email me" . "foo")))

(defvar header-about-example
  "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information.")

(defmacro header-navheader-about (&body body)
  `(spinneret:with-html
     (:div :class "col-sm-8 col-md-7 py-4"
           (:h4 "About")
           (:p :class "text-body-secondary"
               ,@body))))

(defmacro header-navheader-contact (&body body)
  `(spinneret:with-html
     (:div :class "col-sm-4 offset-md-1 py-4"
           (:h4 "Contact")
           (:ul :class "list-unstyled"
                ,@body))))

(defmacro header-navheader (&body body)
  `(spinneret:with-html
     (:div :class "text-bg-dark collapse"
           :id "navbarHeader"
           (:div :class "container"
                 (:div :class "row"
                       ,@body)))))

(defmacro header-navbar-brand ((&key (brand "Brand") (href "#")) &body body)
  `(spinneret:with-html
       (:a :class "navbar-brand d-flex align-items-center"
           :href ,href
           (:strong ,brand)
           ,@body)))

(defmacro header-navbar-toggler ((&key (target "#navbarHeader")) &body body)
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

(defmacro header-navbar (&body body)
  `(spinneret:with-html
       (:div :class "navbar"
             (:div :class "container"
                   ,@body))))

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))

(defun show-header-navheader-contact (contacts)
  (header-navheader-contact (dolist (contact contacts)
                              (:li (:a :href (rest contact)
                                       :class "text-white"
                                       (first contact))))))

(defun show-header-navheader (about contacts)
  (header-navheader (header-navheader-about about)
    (show-header-navheader-contact contacts)))

(defun show-header-navbar (brand href target)
  (header-navbar () (header-navbar-brand (:brand brand :href href))
    (header-navbar-toggler (:target target))))
