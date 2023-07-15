(defpackage cl-sbt-album
  (:use :cl)
  (:import-from :cl-sbt-grid :container :row :col)
  (:import-from :cl-sbt-navbar :navbar :brand :toggler)
  (:export
   :show-album-page
   :header
   :footer
   :main
   :hero
   :album))

(in-package :cl-sbt-album)

(defparameter *navbar-header-id* "navbarHeader")

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

(defmacro navbar-header (&body body)
  `(spinneret:with-html
     (:div :id ,*navbar-header-id*
           :class "collapse"
           ,@body)))

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))

(defmacro main (&body body)
  `(spinneret:with-html
     (:main ,@body)))

(defmacro hero ((&key (headline nil) (cta-1 nil) (cta-2 nil)) &body body)
  `(spinneret:with-html
     (:section :class "py-5 text-center container"
               (:div :class "row py-lg-5"
                     (:div :class "col-lg-6 col-md-8 mx-auto"
                           (:h1 :class "fw-light"
                                ,headline)
                           (:p :class "lead text-body-secondary"
                               ,@body)
                           (:p (:a :class "btn btn-primary my-2"
                                   ,cta-1)
                               (:a :class "btn btn-secondary my-2"
                                   ,cta-2)))))))

(defmacro card (&body body)
  `(spinneret:with-html
     (:div :class "col"
           (:div :class "card shadow-sm"
                 (:div :class "card-body"
                       (:p :class "card-text"
                           ,@body))))))

(defmacro album (&body body)
  `(spinneret:with-html
     (:div :class "album py-5 bg-body-tertiary"
           (:div :class "container"
                 (:div :class "row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3"
                       ,@body)))))

(defun show-album ()
  (album (dolist (card-text '("foo" "bar" "hello" "world"))
           (card card-text))))

(defmacro footer (&body body)
  `(spinneret:with-html
     (:footer :class "text-body-secondary py-5"
       (:div :class "container"
             (:p :class "float-end mb-1"
                 (:a :href "#" "Back to top"))
             (:p :class "mb-0"
                 ,@body)))))

(defun show-album-page (title)
  (cl-sbt:with-page (:title title)
    (header (navbar-header
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
    (main (show-album))
    (footer "bar")))
