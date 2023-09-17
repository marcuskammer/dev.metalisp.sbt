(defpackage cl-sbt/tests/form
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/form
   :ctrl
   :combo
   :combo-sm
   :combo-lg
   :search-form
   :checkable))

(in-package :cl-sbt/tests/form)

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
