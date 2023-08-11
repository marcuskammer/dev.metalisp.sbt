(defpackage cl-sbt/tests/form
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/form
   :ctrl))

(in-package :cl-sbt/tests/form)

(deftest test-simple-form-control
  (let ((result (spinneret:with-html-string (ctrl (:id "exampleFormControlInput1" :label "Email address" :type "email" :placeholder "name@example.com")))))
    (testing "Generates correct HTML for a simple form"
      (ok (search "for=exampleFormControlInput1" result))
      (ok (search "class=form-label" result))
      (ok (search "type=email" result))
      (ok (search "class=form-control" result))
      (ok (search "id=exampleFormControlInput1" result))
      (ok (search "placeholder=name@example.com" result)))))

(deftest test-simple-form-control-with-text
  (let ((result (spinneret:with-html-string (ctrl (:id "exampleFormControlInput1" :label "Email address" :type "email" :placeholder "name@example.com" :text "Your password must be 8-20 characters long" :describeby "passwordHelpBlock")))))
    (testing "Generates correct HTML for a simple form with text"
      (ok (search "for=exampleFormControlInput1" result))
      (ok (search "class=form-label" result))
      (ok (search "type=email" result))
      (ok (search "class=form-control" result))
      (ok (search "id=exampleFormControlInput1" result))
      (ok (search "placeholder=name@example.com" result))
      (ok (search "class=form-text" result))
      (ok (search "Your password must" result))
      (ok (search "aria-describeby=passwordHelpBlock" result))
      (ok (search "id=passwordHelpBlock" result)))))

(deftest test-simple-form-control-2
  (let ((result (spinneret:with-html-string (ctrl (:id "exampleFormControlInput1" :label "Email address" :type "email" :placeholder "name@example.com") (:id "form1" :label "Another Form" :type "email" :placeholder "name@example.com")))))
    (testing "Generates correct HTML for a simple form with more than one entries"
      (ok (search "for=exampleFormControlInput1" result))
      (ok (search "class=form-label" result))
      (ok (search "type=email" result))
      (ok (search "class=form-control" result))
      (ok (search "id=exampleFormControlInput1" result))
      (ok (search "placeholder=name@example.com" result))
      (ok (search "for=form1" result))
      (ok (search "Another Form" result)))))