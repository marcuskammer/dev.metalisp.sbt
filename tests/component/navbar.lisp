(defpackage cl-sbt/tests/navbar
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/navbar
   :navbar
   :brand
   :nav
   :text
   :toggler
   :brand-logo
   :collapsible))

(in-package :cl-sbt/tests/navbar)

(deftest test-brand-logo
  (let ((result (spinneret:with-html-string (brand-logo (:src "logo.png" :alt "My Logo" :width 50 :height 50 :classes "logo-class")))))
    (ok (search "src=logo.png" result))
    (ok (search "alt=\"My Logo\"" result))
    (ok (search "width=50" result))
    (ok (search "height=50" result))
    (ok (search "class=logo-class" result))))

(deftest test-brand
  (let ((result (spinneret:with-html-string (brand (:imgsrc "logo.png") "My Website"))))
    (ok (search "class=navbar-brand" result))
    (ok (search "href=#" result))
    (ok (search "src=logo.png" result))
    (ok (search "alt=Logo" result))
    (ok (search "width=30" result))
    (ok (search "height=24" result))
    (ok (search "class=\"d-inline-block align-text-top\"" result))
    (ok (search "My Website" result))))

(deftest test-nav
  (let ((result (spinneret:with-html-string (nav "Home" "About" "Contact"))))
    (ok (search "class=\"collapse navbar-collapse\"" result))
    (ok (search "id=navbarNav" result))
    (ok (search "Home" result))))

(deftest test-text
  (let ((result (spinneret:with-html-string (text "Welcome to my website!"))))
    (ok (search "class=navbar-text" result))
    (ok (search "Welcome to my website!" result))))

(deftest test-toggler
  (let ((result (spinneret:with-html-string (toggler "myNavbar"))))
    (ok (search "class=\"navbar-toggler collapsed\"" result))
    (ok (search "type=button" result))
    (ok (search "data-bs-toggle=collapse" result))
    (ok (search "data-bs-target=#myNavbar" result))
    (ok (search "aria-controls=myNavbar" result))
    (ok (search "aria-expanded=false" result))
    (ok (search "aria-label=\"Toggle navigation\"" result))
    (ok (search "class=navbar-toggler-icon" result))))

(deftest test-collapsible
  (let ((result (spinneret:with-html-string (collapsible "myId" "red" "Content"))))
    (ok (search "id=myId" result))
    (ok (search "class=\"collapse red\"" result))
    (ok (search "class=container" result))
    (ok (search "class=row" result))))

(deftest test-navbar
  (let ((result (spinneret:with-html-string (navbar (:fluid t :dark t) (brand (:imgsrc "logo.png") "My Website") (nav "Home" "About" "Contact")))))
    (ok (search "class=\"navbar navbar-dark bg-dark\"" result))
    (ok (search "class=container-fluid" result))
    (ok (search "class=navbar-brand" result))
    (ok (search "class=\"collapse navbar-collapse\"" result))
    (ok (search "id=navbarNav" result))))
