(defpackage cl-sbt/tests/form
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/form
   :ctrl
   :select
   :search-form
   :checkable))

(in-package :cl-sbt/tests/form)

(deftest test-select-default
  (let ((result (spinneret:with-html-string (select () (:content "One" :value 1) (:content "Two" :value 2) (:content "Three" :value 3)))))
    (testing "Generates correct HTML for select element"
      (ok (search "class=form-select" result))
      (ok (search "aria-label=\"Default select example\"" result))
      (ok (search "option value=1" result))
      (ok (search "option value=2" result))
      (ok (search "option value=3" result)))))

(deftest test-select-size-lg
  (let ((result (spinneret:with-html-string (select (:size "lg") (:content "One" :value 1)))))
    (testing "Generates correct HTML for select element"
      (ok (search "class=\"form-select form-select-lg\"" result))
      (ok (search "aria-label=\"Default select example\"" result))
      (ok (search "option value=1" result)))))

(deftest test-select-size-sm
  (let ((result (spinneret:with-html-string (select (:size "sm") (:content "One" :value 1)))))
    (testing "Generates correct HTML for select element"
      (ok (search "class=\"form-select form-select-sm\"" result))
      (ok (search "aria-label=\"Default select example\"" result))
      (ok (search "option value=1" result)))))

(deftest test-select-size-multiple
  (let ((result (spinneret:with-html-string (select (:size "multiple") (:content "One" :value 1)))))
    (testing "Generates correct HTML for select element"
      (ok (search "class=form-select" result))
      (ok (search "multiple" result))
      (ok (search "option value=1" result)))))

(deftest test-select-size-number
  (let ((result (spinneret:with-html-string (select (:size 3) (:content "One" :value 1)))))
    (testing "Generates correct HTML for select element"
      (ok (search "class=form-select" result))
      (ok (search "size=3" result))
      (ok (search "option value=1" result)))))

(deftest test-default-search-form
  (let ((result (spinneret:with-html-string (search-form))))
    (testing "Generates correct HTML for search form"
      (ok (search "class=\"form-control me-2\"" result))
      (ok (search "type=search" result))
      (ok (search "aria-label=Search" result))
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
