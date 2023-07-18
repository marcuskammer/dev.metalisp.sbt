;; The Bootstrap example 'Album' demonstrates how to create a responsive,
;; mobile-first photo album layout using Bootstrap's grid system, cards, and
;; some other components. Here's a brief overview of its structure:

;; Navigation Bar: At the top, there's a navbar with the site name and some
;; links. The navbar collapses into a hamburger menu on smaller screens for
;; better mobile usability.

;; Hero Unit: The main headline or 'Hero unit' is a large callout with some
;; text and a call-to-action button. It's a marketing section of the page that
;; offers users a brief overview of the product's main feature.

;; Photo Album: The main content of the page is a photo album. This is
;; implemented as a responsive grid of card components. Each card contains an
;; image, some text, and a button. The grid adjusts from a single column on the
;; smallest screens to multi-column layouts on larger screens.

;; Footer: Finally, there's a footer at the bottom of the page with some links
;; and copyright information.

;; The Album example is a good demonstration of how Bootstrap can be used to
;; create a responsive, mobile-first layout with relatively little code. It
;; shows how to use some of the most common Bootstrap components, like navbars,
;; cards, and the grid system, and it provides a solid basis for further
;; customization and extension.

;; It's worth noting that while the layout is quite simple, it can be used as a
;; starting point for more complex projects. By understanding and working
;; through this example, one can get a grasp of Bootstrap's conventions and
;; learn how to use its components effectively.

(defpackage cl-sbt/album
  (:use :cl)
  (:import-from :cl-sbt/grid :con :row :col)
  (:import-from :cl-sbt/navbar :navbar :brand :toggler)
  (:import-from :cl-sbt :write-string-to-file :with-page)
  (:import-from :spinneret :with-html-string)
  (:export
   :contact
   :about
   :header
   :footer
   :album
   :*navbar-header-id*)
  (:documentation "The `cl-sbt-album` package provides macros for building an
  album-style website page using Bootstrap and Spinneret."))

(in-package :cl-sbt/album)

(defvar *navbar-header-id* "navbarHeader")

(defmacro about ((&key (textbody "secondary")) &body body)
  "Generates an HTML 'About' section with the provided content.

TEXTBODY: Specifies the color scheme of the text body. Default is 'secondary'.

BODY: Specifies the HTML content to be included in the 'About' section. This
can be any valid HTML content that spinneret:with-html can parse.

Example usage:
  (about () \"This is an about section.\")
  ; This will generate an 'About' section with secondary color text and the
  ; provided content."
  `(spinneret:with-html
     (:h4 "About"
          (:p :class ,(concatenate 'string
                                   (if textbody (format nil "text-body-~a" textbody) ""))
              ,@body))))

(defmacro contact (&rest rest)
  "Generates an HTML 'Contact' section with the provided links.

REST: A list of plists, each representing a link. Each plist should contain a
:url and a :label.

Example usage:
  (contact (:url \"#\" :label \"Follow on Twitter\")
           (:url \"#\" :label \"Like on Facebook\")
           (:url \"#\" :label \"Email me\"))
  ; This will generate a 'Contact' section with three links, each with the
  ; provided URL and label."
  `(spinneret:with-html
     (:h4 "Contact")
     (:ul :class "list-unstyled"
          ,@(loop for item in rest
                  collect (destructuring-bind (&key url label) item
                            `(:li (:a :class "text-white"
                                      :href ,url
                                      ,label)))))))

(defmacro navigation (&body body)
  "Generates an HTML header for the album page.

This macro generates a header with a navigation bar and some predefined
content, including an 'About' section with a description of the album and a
'Contact' section with some dummy contact links.

Additional content can be added to the header by passing it as BODY arguments
to the macro. The BODY content will be included in the header after the
predefined content."
  `(spinneret:with-html
     (:header (:div :id ,*navbar-header-id*
                    :class "collapse"
                    (con ()
                      (row ()
                        (col (:breakpoint (:kind :col :sm (8 nil) :md (7 nil))
                              :spacing (:property :p :side :y :size 4))
                          (about () "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information."))
                        (col (:breakpoint (:kind :col :sm (8 nil) :md (nil 1)))
                          (contact (:url "#" :label "Follow on Twitter")
                                   (:url "#" :label "Like on Facebook")
                                   (:url "#" :label "Email me"))))))
       (navbar (:fluid nil)
         (brand () "Album")
         (toggler ,*navbar-header-id*))
       ,@body)))

(defmacro footer ((&key (color nil) (spacing nil)) &body body)
  "Generates an HTML footer with Bootstrap classes.

COLOR: Specifies the color scheme of the footer. It's a list containing keyword
arguments that can be passed to the cl-sbt/utility:color function.

SPACING: A list specifying the Bootstrap spacing class. The list should contain
keyword arguments that can be passed to the cl-sbt/utility:spacing function.

BODY: Optional. Specifies additional HTML content to be added to the footer.
This can be any valid HTML content that spinneret:with-html can parse.

The footer generated contains fixed content, including a 'Back to top' link and
a short paragraph about Bootstrap.

Example usage:
  (footer (:color (:text :primary :background :light)
          :spacing (:property :p :side :y :size 4))
          (:p :class \"custom-class\" \"Custom content here\"))
  ; This will generate a footer with primary color text and light background
  ; with a top/bottom padding of size 4. Additionally, a paragraph with class
  ; 'custom-class' and text 'Custom content here' will be added to the footer."
  `(spinneret:with-html
     (:footer :class ,(concatenate 'string
                                   (if (null color) ""
                                       (apply #'cl-sbt/utility:color color))
                                   (if (null spacing) ""
                                       (apply #'cl-sbt/utility:spacing spacing)))
              (con ()
                (:p :class "float-end mb-1"
                    (:a :href "#" "Back to top"))
                (:p :class "mb-1"
                    "Album example is © Bootstrap, but please download and customize it for yourself!")
                (:p :class "mb-0"
                    "New to Bootstrap? "
                    (:a :href "/" "Visit the homepage")
                    " or read our "
                    (:a :href "/docs/5.3/getting-started/introduction/" "getting started guide"))
                ,@body))))

(defmacro hero (&body body)
  `(spinneret:with-html
     (con (:spacing (:property :p :size 5))
       (row (:spacing (:property :p :side :lg :size 5))
         (col (:breakpoint (:kind :col :md (8 nil) :lg (6 nil))
               :spacing (:property :p :size 5))
           ,@body)))))

(defmacro page (title &body body)
  `(with-page (:title ,title)
     (header)
     (:main ,@body)
     (footer (:spacing (:property :p :side :y :size 5)
              :color (:text "body-secondary")))))

(defun write-album (&key (lang "de") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (write-string-to-file "album.html"
                          (with-html-string (page "Album" (hero))))))
