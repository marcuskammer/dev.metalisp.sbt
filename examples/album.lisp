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
  (:import-from :cl-sbt/btn :btn-primary)
  (:import-from :cl-sbt/navbar :navbar :brand :toggler :collapsible)
  (:import-from :cl-sbt/utility :color)
  (:import-from :cl-sbt :write-string-to-file :with-page)
  (:import-from :spinneret :with-html-string)
  (:export
   :contact
   :about
   :header
   :footer
   :album
   :page
   :write-page
   :*navbar-header-id*)
  (:documentation "The `cl-sbt-album` package provides macros for building an
  album-style website page using Bootstrap and Spinneret."))

(in-package :cl-sbt/album)

(defvar *navbar-header-id* "navbarHeader")

(defmacro about ((&key (color '(:text :body-secondary))) &body body)
  "Generates an HTML 'About' section with the provided content.

COLOR: Specifies the color scheme of the text.

BODY: Specifies the HTML content to be included in the 'About' section. This
can be any valid HTML content that spinneret:with-html can parse.

Example usage:
  (about () \"This is an about section.\")
  ; This will generate an 'About' section with secondary color text and the
  ; provided content."
  `(spinneret:with-html
     (:h4 "About")
     (:p :class ,(concatenate 'string
                              (if color (apply #'cl-sbt/utility:color color) ""))
         ,@body)))

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
                            `(:li (:a :class ,(cl-sbt/utility:color :text :white)
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
     (:header (collapsible *navbar-header-id*
                ,@body)
       (navbar (:fluid nil)
         (brand () "Album")
         (toggler *navbar-header-id*)))))

(defmacro footer ((&key (color '(:text :body-secondary)) (spacing '(:property :p :size 5))) &rest rest)
  "Generates an HTML footer with Bootstrap classes.

COLOR: Specifies the color scheme of the footer. It's a list containing keyword
arguments that can be passed to the cl-sbt/utility:color function. Default is
'(:text :body-secondary), setting the text color to secondary.

SPACING: A list specifying the Bootstrap spacing class. The list should contain
keyword arguments that can be passed to the cl-sbt/utility:spacing function.
Default is '(:property :p :size 5), setting the padding size to 5.

REST: A list of key-value pairs. A recognized key is :copyright which
sets the copyright notice in the footer. If not provided, it defaults to nil.
Specifies additional HTML content to be added to the footer. This can be any
valid HTML content that spinneret:with-html can parse.

The footer generated contains fixed content, including a 'Back to top' link and
a short paragraph about Bootstrap.

Example usage:
  (footer (:color (:text :primary :background :light)
          :spacing (:property :p :side :y :size 4))
          :copyright \"Copyright 2023\"
          (:p :class \"custom-class\" \"Custom content here\"))
  ; This will generate a footer with primary color text and light background
  ; with a top/bottom padding of size 4. A copyright notice is set. Additionally,
  ; a paragraph with class 'custom-class' and text 'Custom content here' will
  ; be added to the footer."
  `(spinneret:with-html
     (:footer :class ,(concatenate 'string
                                   (if color (apply #'cl-sbt/utility:color color) "")
                                   (if spacing (apply #'cl-sbt/utility:spacing spacing) ""))
              (con nil
                ,@(destructuring-bind (&key copyright content) rest
                    `((:p :class "float-end mb-1"
                          (:a :href "#" "Back to top"))
                      (:p :class "mb-1"
                          ,copyright)
                      (:p :class "mb-0"
                          "New to Bootstrap? "
                          (:a :href "/" "Visit the homepage")
                          " or read our "
                          (:a :href "/docs/5.3/getting-started/introduction/" "getting started guide"))
                      ,content))))))

(defmacro hero (&rest rest)
  "Generates a Bootstrap hero unit.

REST: A list of key-value pairs. The keys can be:
  :title - The title of the hero unit.
  :lead - The leading text of the hero unit.
  :cta - The text for the call to action button.

This macro generates a hero unit with the given title, leading text, and call
to action button. The generated hero unit uses Bootstrap classes for styling."
  `(spinneret:with-html
     ,@(destructuring-bind (&key title lead cta) rest
         `((:h1 :class "fw-light" ,title)
           (lead-p () ,lead)
           (:p (btn-primary ,cta))))))

(defmacro lead-p ((&key (color '(:text "body-secondary"))) &body body)
  `(spinneret:with-html
              (:p :class ,(format nil "lead ~a" (if color (apply #'color color)))
                  ,@body)))

(defmacro page (title cdn)
  "Generates a complete HTML page using Bootstrap components.

TITLE: The title of the page.

CDN: The Content Delivery Network (CDN) link for Bootstrap.

This macro generates a page that includes a navigation bar with an 'about'
section and a 'contact' section, a main section with a hero unit, a leading
paragraph, and a 'call to action' button, and a footer with copyright info.

Example usage:
  (page \"My Page Title\" t)
  ; This will generate an HTML page with the title 'My Page Title', and the
  ; Bootstrap CSS will be loaded from the provided CDN link.

The 'about' section of the navigation bar contains a paragraph of text. The
'contact' section includes links to Twitter, Facebook, and an email address.

The hero unit in the main section includes a title, a lead paragraph, and a
'call to action' button.

The footer contains a copyright notice.

All of these components are customizable by providing different arguments
to the respective component macros."
  `(with-page (:cdn ,cdn :title ,title)
     (navigation
       (col (:breakpoint (:kind :col :sm (8 nil) :md (7 nil))
             :spacing (:property :p :side :y :size 4))
         (about (:color (:text "body-secondary"))
           "Add some information about the album below, the author, or any
           other background context. Make it a few sentences long so folks can
           pick up some informative tidbits. Then, link them off to some social
           networking sites or contact information."))

       (col (:breakpoint (:kind :col :sm (4 nil) :md (nil 1)))
         (contact (:url "#" :label "Follow on Twitter")
                  (:url "#" :label "Like on Facebook")
                  (:url "#" :label "Email me"))))

     (:main (hero :title "Album example"
                  :lead "Something short and leading about the collection
                  below—its contents, the creator, etc. Make it short and
                  sweet, but not too short so folks don’t simply skip over it
                  entirely."
                  :cta "Main call to action"))

     (footer nil :copyright "Album example is © Bootstrap, but please download and customize it for yourself!")))

(defun write-page (filepath &key (lang "de") (style :tree) (fc 120))
  (let ((spinneret:*html-lang* lang)
        (spinneret:*html-style* style)
        (spinneret:*fill-column* fc))
    (write-string-to-file filepath
                          (with-html-string (page "Album" t)))))

(cl-sbt/album:write-page "~/quicklisp/local-projects/cl-sbt/public/examples/album.html")
