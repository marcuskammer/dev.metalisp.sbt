(in-package :cl-sbt)

(load "src/examples/album/_navbar.lisp")
(load "src/examples/album/_main.lisp")

(defvar contact-examples
  '(("Follow on Twitter" . "foo")
    ("Like on Facebook" . "foo")
    ("Email me" . "foo")))

(defvar about-example
  "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information.")

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))

(defmacro footer (&body body)
  `(spinneret:with-html
     (:footer ,@body)))

(defun show-album-page (title)
  (with-page (:title title)
    (header (show-navbar-header about-example contact-examples)
      (show-navbar "brand" "#" "foo"))
    (main "foo")
    (footer "bar")))
