(defpackage ml-sbt/tests/dropdown
  (:use
   :cl
   :ml-sbt/dropdown
   :rove))

(in-package :ml-sbt/tests/dropdown)

(deftest test-menu
  (let ((result (spinneret:with-html-string (menu (item "Item 1") (item "Item 2")))))
    (testing "Generates correct HTML for a menu with items"
      (ok (search "class=dropdown-menu" result))
      (ok (search "class=dropdown-item" result))
      (ok (search "Item 1" result))
      (ok (search "Item 2" result)))))

(deftest test-item
  (let ((result (spinneret:with-html-string (item "Item 1"))))
    (testing "Generates correct HTML for an item"
      (ok (search "class=dropdown-item" result))
      (ok (search "Item 1" result)))))

(deftest test-dropdown
  (let ((result (spinneret:with-html-string (dropdown (:name "Dropdown") (menu (item "Item 1") (item "Item 2"))))))
    (testing "Generates correct HTML for a dropdown with title and menu"
      (ok (search "class=dropdown" result))
      (ok (search "class=\"btn btn-secondary dropdown-toggle\"" result))
      (ok (search "type=button" result))
      (ok (search "data-bs-toggle=dropdown" result))
      (ok (search "aria-expanded=false" result))
      (ok (search "class=dropdown-menu" result))
      (ok (search "class=dropdown-item" result))
      (ok (search "Item 1" result))
      (ok (search "Item 2" result)))))
