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

(defpackage cl-sbt/navbar
  (:use :cl)
  (:export
   :navbar
   :brand
   :nav
   :text
   :toggler
   :brand-logo))

(in-package :cl-sbt/navbar)

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

(defmacro brand ((&key (logo-src nil)) &body body)
  "This macro generates a brand component for a Bootstrap navbar.

LOGO-SRC: Specifies the URL of the logo image. If not provided, no logo will be displayed.
BODY: Specifies the text or other content to be displayed in the brand component.

Example:
 (brand (:logo-src \"logo.png\") \"My Website\")"
  `(spinneret:with-html
     (:a :class "navbar-brand"
         :href "#"
         ,(if (null logo-src) nil `(navbar-brand-logo ,logo-src))
         ,@body)))

(defmacro nav (&body body)
  "This macro generates a navigation component for a Bootstrap navbar.

BODY: Specifies the content to be displayed in the navigation component.

Example:
  (nav \"Home\" \"About\" \"Contact\")"
  `(spinneret:with-html
     (:div :class "collapse navbar-collaps"
           :id "navbarNav"
           ,@body)))

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

(defmacro navbar ((&key (fluid t) (classes "")) &body body)
  "This macro generates a Bootstrap navbar.

FLUID: Specifies whether the navbar should be full width. Defaults to true.
CLASSES: Specifies additional CSS classes for the navbar.
BODY: Specifies the content to be displayed in the navbar.

Example:
 (navbar (:fluid t :classes \"navbar-light bg-light\")
   (brand \"My Website\")
   (nav \"Home\" \"About\" \"Contact\"))"
  `(spinneret:with-html
     (:nav :class (format nil "navbar ~a" ,classes)
           (:div :class ,(if fluid
                             "container-fluid"
                             "container")
                 ,@body))))
