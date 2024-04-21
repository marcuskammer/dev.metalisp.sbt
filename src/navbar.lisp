;; https://getbootstrap.com/docs/5.3/components/navbar/

;; Bootstrap's Navbar component is a responsive, mobile-first navigation header.
;; It includes built-in support for branding, navigation, and more. It can include
;; various elements such as links, forms, buttons, text, etc.

;; Some key features of the Bootstrap Navbar component include:

;; Branding: You can easily include a brand logo or brand name in the Navbar. This
;; is typically placed on the left of the bar.

;; Navigation: The primary function of the Navbar is to serve as a navigation bar.
;; It provides support for a set of links that can be used to navigate your site.

;; Responsiveness: The Navbar is designed to be responsive. On smaller screens,
;; the navigation links collapse into a dropdown menu.

;; Customizability: The appearance of the Navbar can be customized extensively
;; using Bootstrap's predefined classes. This includes changes to the color
;; scheme, positioning of elements, and size of the Navbar.

;; Dropdowns: Navbar component also supports dropdown menus. This allows you to
;; group related links under one heading.

;; Form Support: You can include form elements directly in your Navbar, allowing
;; users to input data directly from the navigation bar.

(defpackage dev.metalisp.sbt/navbar
  (:use :cl)
  (:import-from
   :dev.metalisp.sbt/btn
   :btn-outline-success)
  (:import-from
   :dev.metalisp.sbt/grid
   :con
   :row)
  (:export
   :navbar
   :brand
   :nav
   :text
   :toggler
   :brand-logo
   :content
   :form-search
   :collapsible))

(in-package :dev.metalisp.sbt/navbar)

(defmacro brand-logo ((&key (src "#") (alt "Logo") (width 30) (height 24) (classes "d-inline-block align-text-top")))
  "This macro generates a brand logo for a Bootstrap navbar.

SRC: Specifies the URL of the logo image. Defaults to '#'.
ALT: Specifies the alt text for the logo. Defaults to 'Logo'.
WIDTH: Specifies the width of the logo. Defaults to 30.
HEIGHT: Specifies the height of the logo. Defaults to 24.
CLASSES: Specifies additional CSS classes for the logo. Defaults to 'd-inline-block align-text-top'.

Example:
 (brand-logo (:src \"logo.png\" :alt \"My Website Logo\" :width 50 :height 40))"
  `(spinneret:with-html
     (:img :src ,src
           :alt ,alt
           :width ,width
           :height ,height
           :class ,classes)))

(defmacro brand ((&key (logo nil)) &body body)
  "This macro generates a brand component for a Bootstrap navbar.

LOGO-SRC: Specifies the URL of the logo image. If not provided, no logo will be displayed.
BODY: Specifies the text or other content to be displayed in the brand component.

Example:
 (brand (:logo-src \"logo.png\") \"My Website\")"
  `(spinneret:with-html
     (:a :class "navbar-brand"
         :href "#"
         ,(if (null logo) nil `(brand-logo (:src ,logo)))
         ,@body)))

(defmacro nav (&rest rest)
  "This macro generates a navigation component for a Bootstrap navbar.

BODY: Specifies the content to be displayed in the navigation component.

Example:
  (nav \"Home\" \"About\" \"Contact\")"
  `(spinneret:with-html
     (:ul :class "navbar-nav"
          ,@(loop for item in rest
                  collect (destructuring-bind (&key name url (active nil) (disabled nil)) item
                            `(:li :class "nav-item"
                                  ,(cond (active `(:a :class "nav-link active" :aria-current "page" :href ,url ,name))
                                         (disabled `(:a :class "nav-link disabled" :aria-disabled "true" :href ,url ,name))
                                         (t `(:a :class "nav-link" :href ,url ,name)))))))))

(defmacro text (&body body)
  "This macro generates a text component for a Bootstrap navbar.

BODY: Specifies the text to be displayed in the text component.

Example:
 (text \"Welcome to my website!\")"
  `(spinneret:with-html
     (:span :class "navbar-text"
            ,@body)))

(defmacro toggler (target)
  "Generates a Bootstrap navigation bar toggler button.

TARGET: Specifies the ID of the element that will be toggled when the button is clicked.

The button is designed to work with a collapsible navigation bar. It will add
the Bootstrap classes and data attributes required to make the button
functional.

Example:
  (toggler \"myNavbar\")"
  `(spinneret:with-html
     (:button :class "navbar-toggler collapsed"
              :type "button"
              :data-bs-toggle "collapse"
              :data-bs-target (format nil "#~a" ,target)
              :aria-controls ,target
              :aria-expanded "false"
              :aria-label "Toggle navigation"
              (:span :class "navbar-toggler-icon"))))

(defmacro collapsible (id color &body body)
  `(spinneret:with-html
     (:div :id ,id
           :class ,(format nil "collapse ~a" color)
           (con ()
             (row ()
               ,@body)))))

(defmacro form-search ()
  `(spinneret:with-html
     (:form :class "d-flex"
            :role "search"
            (:input :class "form-control me-2"
                    :type "search"
                    :placeholder "Search"
                    :aria-label "Search")
            (btn-outline-success () "Search"))))

(defmacro content (id (&key (search t)) &body body)
  `(spinneret:with-html
     (:div :class "navbar-collapse collapse"
           :id ,id
           ,@body
           ,(if search `(form-search) nil))))

(defmacro navbar ((&key (fluid t) (expand nil)) &body body)
  "This macro generates a Bootstrap navbar.

FLUID: Specifies whether the navbar should be full width. Defaults to true.

EXPAND: Specifies the breakpoint at which the navbar will be expanded.

BODY: Specifies the content to be displayed in the navbar.

Example:
 (navbar (:fluid t :expand \"lg\")
   (brand () \"My Website\")
   (content \"navbarContent\" (nav (:name \"Home\" :url \"#\" :active t)
                 (:name \"Foo\" :url \"#\")
                 (:name \"Bar\" :url \"#\" :disabled t))))"
  `(spinneret:with-html
     (:nav :class ,(concatenate 'string
                                "navbar "
                                (if (null expand)
                                    nil
                                    (format nil "navbar-expand-~a" expand)))
           (:div :class ,(if fluid
                             "container-fluid"
                             "container")
                 ,@body))))
