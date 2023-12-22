(defpackage dev.metalisp.sbt/tests/form
  (:use
   :cl
   :dev.metalisp.sbt
   :rove)
  (:import-from
   :dev.metalisp.sbt/component/form
   :remove-special-chars
   :clean-form-str
   :build-name-str
   :build-value-str
   :build-value-prop-str
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

(deftest test-clean-form-str
  (testing "Cleans a form string for use as a name or identifier."
    (ok (string= (clean-form-str " hello WORLD") "hello-world"))))

(deftest test-build-name-str
  (testing "Builds a standardized string by adding a 'group-' prefix and applying cleaning functions."
    (ok (string= (build-name-str "somename") "group-somename"))))

(deftest test-build-value-str
  (testing "Trims leading and trailing spaces from the given value string."
    (ok (string= (build-value-str " hello-world ") "hello-world"))))

(deftest test-build-value-prop-str
  (testing "Builds a value property string by applying various cleaning functions."
    (ok (string= (build-value-prop-str " hello world ") "hello-world"))))

(deftest test-ctrl-number
  (let ((result (spinneret:with-html-string (ctrl "number" "testform" "Birth Date"))))
    (testing "Generates multiple checkable functions based on the provided list of types."
      (ok (search "type=number" result)))))

(deftest test-ctrl-email
  (let ((result (spinneret:with-html-string (ctrl "email" "testform" "E-Mail"))))
    (testing "Generates multiple checkable functions based on the provided list of types."
      (ok (search "type=email" result)))))

(deftest test-ctrl-tel
  (let ((result (spinneret:with-html-string (ctrl "tel" "testform" "Phone number"))))
    (testing "Generates multiple checkable functions based on the provided list of types."
      (ok (search "type=tel" result)))))

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
