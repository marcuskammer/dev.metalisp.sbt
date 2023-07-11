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

(defpackage cl-sbt-navbar
  (:use :cl)
  (:export
   :navbar
   :brand
   :nav
   :text
   :brand-logo))

(in-package cl-sbt-navbar)

(defmacro brand-logo ((&key (src "#") (alt "Logo") (width 30) (height 24) (classes "d-inline-block align-text-top")))
  "This macro generates the HTML code for a Bootstrap navbar brand logo.

The NAVBAR-BRAND-LOGO macro takes the following keyword arguments:

  - :src - a string specifying the source URL of the logo image.
  - :alt - a string specifying the alternative text for the logo image. Default is 'Logo'.
  - :width - an integer specifying the width of the logo image. Default is 30.
  - :height - an integer specifying the height of the logo image. Default is 24.
  - :classes - a string specifying the CSS classes for the logo image. Default is 'd-inline-block align-text-top'.

The generated HTML code includes an <img> element with the specified parameters."

  `(spinneret:with-html
     (:img :src ,src
           :alt ,alt
           :width ,width
           :height ,height
           :class ,classes)))

(defmacro brand ((&key (logo-src nil)) &body body)
  "This macro generates the HTML code for a Bootstrap navbar brand.

The NAVBAR-BRAND macro takes the following keyword arguments:

  - :logo-src - a string specifying the source URL of the logo image. If provided, a call to the `navbar-brand-logo` macro is included to add a logo to the navbar brand.

The macro also takes an arbitrary number of body expressions, which are included in the generated HTML code.

The generated HTML code includes an <a> element with the CSS class 'navbar-brand'. If a logo source is provided, an <img> element for the logo is also included in the <a> element."

  `(spinneret:with-html
     (:a :class "navbar-brand"
         :href "#"
         ,(if (null logo-src) nil `(navbar-brand-logo ,logo-src))
         ,@body)))

(defmacro nav (&body body)
  "This macro generates the HTML code for a Bootstrap navbar navigation.

The NAVBAR-NAV macro takes an arbitrary number of body expressions, which are included in the generated HTML code.

The generated HTML code includes a <div> element with the CSS class 'collapse navbar-collaps' and id 'navbarNav'."

  `(spinneret:with-html
     (:div :class "collapse navbar-collaps"
           :id "navbarNav"
           ,@body)))

(defmacro text (&body body)
  "This macro generates the HTML code for a Bootstrap navbar text.

The NAVBAR-TEXT macro takes an arbitrary number of body expressions, which are included in the generated HTML code.

The generated HTML code includes a <span> element with the CSS class 'navbar-text'."

  `(spinneret:with-html
     (:span :class "navbar-text"
            ,@body)))

(defmacro navbar ((&key (fluid t) (classes "")) &body body)
  "This macro generates the HTML code for a Bootstrap navigation bar.
The NAVBAR macro takes two optional keyword arguments:

  - :fluid (default is t). If non-nil, the generated HTML will use 'container-fluid', otherwise 'container' is used.
  - :classes (default is empty string). If provided, this should be a string of additional CSS classes that will be added to the 'navbar' class of the nav element.

The body of the NAVBAR macro will be included inside the 'container' or 'container-fluid' div."

  `(spinneret:with-html
     (:nav :class (format nil "navbar ~a" ,classes)
           (:div :class ,(if fluid
                             "container-fluid"
                             "container")
                 ,@body))))
