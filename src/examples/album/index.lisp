(in-package :cl-sbt)

(load "src/examples/album/_navbar.lisp")

(defvar contact-examples
  '(("Follow on Twitter" . "foo")
    ("Like on Facebook" . "foo")
    ("Email me" . "foo")))

(defvar about-example
  "Add some information about the album below, the author, or any other background context. Make it a few sentences long so folks can pick up some informative tidbits. Then, link them off to some social networking sites or contact information.")

(defmacro header (&body body)
  `(spinneret:with-html
     (:header ,@body)))

(defun show-album-page ()
  (with-page (:title "foo")
    (header (show-navbar-header about-example contact-examples)
      (show-navbar "brand" "#" "foo"))))
