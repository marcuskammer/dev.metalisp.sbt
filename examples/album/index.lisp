(defpackage cl-sbt-album
  (:use :cl)
  (:export
   :show-album-page
   :header
   :footer
   :main
   :hero
   :album
   :navbar
   :show-navbar))

(in-package :cl-sbt-album)

(defvar contact-examples
  '(("Follow on Twitter" . "foo")
    ("Like on Facebook" . "foo")
    ("Email me" . "foo")))

(defvar about-example
  "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information.")

(defun show-album-page (title)
  (cl-sbt:with-page (:title title)
    (header (show-navbar-header about-example contact-examples)
      (show-navbar "brand" "#" "foo"))
    (main (show-album))
    (footer "bar")))
