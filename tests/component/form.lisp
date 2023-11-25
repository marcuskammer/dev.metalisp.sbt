(defpackage dev.metalisp.sbt/tests/form
  (:use
   :cl
   :dev.metalisp.sbt
   :rove)
  (:import-from
   :dev.metalisp.sbt/form
   :remove-special-chars
   :ctrl
   :combo
   :combo-sm
   :combo-lg
   :search-form
   :checkable))

(in-package :dev.metalisp.sbt/tests/form)

(deftest test-remove-special-chars
  (testing "Removes all special characters from the string STR except numbers and alphabets."
    (ok (string= (remove-special-chars "a1b!@#$%^&*()c2") "a1bc2"))))

(deftest test-select-default
  (let ((result (spinneret:with-html-string (combo () "Red" "Green" "Blue"))))
    (testing "Generates correct HTML for select element"
      (ok (search "class=form-select" result))
      (ok (search "option value=red" result))
      (ok (search "option value=green" result))
      (ok (search "option value=blue" result)))))

(deftest test-default-search-form
  (let ((result (spinneret:with-html-string (search-form))))
    (testing "Generates correct HTML for search form"
      (ok (search "class=\"form-control me-2\"" result))
      (ok (search "type=search" result))
      (ok (search "title=Search" result))
      (ok (search "type=submit" result))
      (ok (search "class=\"btn btn-outline-success\"" result))
      (ok (search "<input" result))
      (ok (search "<button" result)))))

(deftest test-checkable
  (let ((result (spinneret:with-html-string (checkable "radio" "age" "18-24"))))
    (testing "Generates correct HTML for checkable"
      (ok (search "class=\"form-check-label group-age\"" result))
      (ok (search "value=18-24" result))
      (ok (search "type=radio" result))
      (ok (search "name=group-age" result)))))
