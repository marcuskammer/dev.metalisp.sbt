(defpackage cl-sbt-album
  (:use :cl)
  (:import-from :cl-sbt-grid :container :row :col)
  (:import-from :cl-sbt-navbar :navbar :brand :toggler)
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

(defmacro footer (&body body)
  `(spinneret:with-html
     (:footer :class "text-body-secondary py-5"
       ,@body)))

(defmacro album-page (title &body body)
  `(cl-sbt:with-page (:title title)
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
     (footer (container ()
               (:p :class "float-end mb-1"
                   (:a :href "#" "Back to top"))
               (:p :class "mb-0")))))
