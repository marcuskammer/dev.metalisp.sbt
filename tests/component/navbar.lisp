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
   :content
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
  (let ((result (spinneret:with-html-string (brand (:logo "logo.png") "My Website"))))
    (ok (search "class=navbar-brand" result))
    (ok (search "href=#" result))
    (ok (search "src=logo.png" result))
    (ok (search "alt=Logo" result))
    (ok (search "width=30" result))
    (ok (search "height=24" result))
    (ok (search "class=\"d-inline-block align-text-top\"" result))
    (ok (search "My Website" result))))

(deftest test-nav
  (let ((result (spinneret:with-html-string (nav (:name "Home" :url "#" :active t)
                                                 (:name "Foo" :url "#")
                                                 (:name "Bar" :url "#" :disabled t)))))
    (ok (search "Home" result))
    (ok (search "Foo" result))
    (ok (search "Bar" result))
    (ok (search "class=nav-item" result))
    (ok (search "class=\"nav-link active\"" result))
    (ok (search "class=\"nav-link disabled\"" result))
    (ok (search "class=navbar-nav" result))
    (ok (search "aria-current=page" result))
    (ok (search "aria-disabled=true" result))))

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

(deftest test-navbar-simple
  (let ((result (spinneret:with-html-string (navbar () (brand () "My Website") (content "navbarContent" (nav (:name "Home" :url "#" :active t) (:name "Foo" :url "#")))))))
    (ok (search "class=navbar" result))
    (ok (search "class=container" result))
    (ok (search "class=navbar-brand" result))
    (ok (search "class=\"navbar-collapse collapse\"" result))
    (ok (search "id=navbarContent" result))
    (ok (search "class=nav-link" result))
    (ok (search "class=nav-item" result))
    (ok (search "Home" result))
    (ok (search "class=\"nav-link active\"" result))))

(deftest test-navbar-advanced
  (let ((result (spinneret:with-html-string (navbar (:fluid t) (brand (:logo "foobar.png") "My Website") (content "navbarContent" (nav (:name "Home" :url "#" :active t) (:name "Foo" :url "#") (:name "Bar" :url "#" :disabled t)))))))
    (ok (search "class=navbar" result))
    (ok (search "class=container-fluid" result))
    (ok (search "class=navbar-brand" result))
    (ok (search "class=\"navbar-collapse collapse\"" result))
    (ok (search "id=navbarContent" result))
    (ok (search "class=nav-link" result))
    (ok (search "class=nav-item" result))
    (ok (search "Home" result))
    (ok (search "class=\"nav-link active\"" result))
    (ok (search "class=\"nav-link disabled\"" result))
    (ok (search "class=\"d-inline-block align-text-top\"" result))
    (ok (search "src=foobar.png" result))
    (ok (search "aria-disabled=true" result))))
