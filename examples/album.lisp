(defpackage cl-sbt-album
  (:use :cl)
  (:import-from :cl-sbt-grid :container :row :col)
  (:import-from :cl-sbt-navbar :navbar :brand :toggler)
  (:import-from :cl-sbt :write-string-to-file)
  (:import-from :spinneret :with-html-string)
  (:export
   :show-album-page))

(in-package :cl-sbt-album)

(defvar *navbar-header-id* "navbarHeader")

(defmacro about (&body body)
  `(spinneret:with-html
     (:h4 "About"
          (:p :class "text-body-secondary"
              ,@body))))

(defmacro contact (&rest rest)
  `(spinneret:with-html
     (:h4 "Contact")
     (:ul :class "list-unstyled"
          ,@(loop for item in rest
                  collect (destructuring-bind (&key url label) item
                            `(:li (:a :class "text-white"
                                      :href ,url
                                      ,label)))))))

(defmacro navbar-header (id &body body)
  `(spinneret:with-html
     (:div :id ,id
           :class "collapse"
           ,@body)))

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))

(defmacro main (&body body)
  `(spinneret:with-html
     (:main ,@body)))

(defmacro footer ((&key (textbody "secondary") (spacing nil)) &body body)
  "Generates an HTML footer with Bootstrap classes.

TEXTBODY: Specifies the color scheme of the text body, default is 'secondary'.

SPACING: A list specifying the Bootstrap spacing class. The list should contain
keyword arguments that can be passed to the cl-sbt-spacing:spacing function.

BODY: Optional. Specifies additional HTML content to be added to the footer.
This can be any valid HTML content that spinneret:with-html can parse.

The footer generated contains fixed content, including a 'Back to top' link and
a short paragraph about Bootstrap.

Example usage:
  (footer (:textbody \"primary\" :spacing (:property :p :side :y :size 4))
          (:p :class \"custom-class\" \"Custom content here\"))
  ; This will generate a footer with primary color text and a top/bottom
  ; padding of size 4. Additionally, a paragraph with class 'custom-class'
  ; and text 'Custom content here' will be added to the footer."
  `(spinneret:with-html
     (:footer :class ,(concatenate 'string
                                   (if textbody (format nil "text-body-~a " textbody) "")
                                   (if (null spacing) ""
                                       (apply #'cl-sbt-spacing:spacing spacing)))
              (container ()
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

(defmacro album-page (title &body body)
  `(cl-sbt:with-page (:title ,title)
     (header (navbar-header *navbar-header-id*
               (container ()
                 (row ()
                   (col (:sm (8 nil) :md (7 nil))
                     (about "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information."))
                   (col (:sm (8 nil) :md (nil 1))
                     (contact (:url "#" :label "Follow on Twitter")
                              (:url "#" :label "Like on Facebook")
                              (:url "#" :label "Email me"))))))
       (navbar (:fluid nil)
         (brand () "Album")
         (toggler *navbar-header-id*)))
     (main ,@body)
     (footer (:spacing (:property :p :side :y :size 4)))))
