(defpackage cl-sbt/tests/form
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/form
   :form-ctrl))

(in-package :cl-sbt/tests/form)

(deftest test-simple-form-control
  (let ((result (spinneret:with-html-string (form-ctrl (:id "exampleFormControlInput1" :label "Email address" :type "email" :placeholder "name@example.com")))))
    (testing "Generates correct HTML for a simple form"
      (ok (search "for=exampleFormControlInput1" result))
      (ok (search "class=form-label" result))
      (ok (search "type=email" result))
      (ok (search "class=form-control" result))
      (ok (search "id=exampleFormControlInput1" result))
      (ok (search "placeholder=name@example.com" result)))))

(deftest test-simple-form-control-2
  (let ((result (spinneret:with-html-string (form-ctrl
                                             (:id "exampleFormControlInput1"
                                              :label "Email address"
                                              :type "email"
                                              :placeholder "name@example.com")
                                             (:id "form1"
                                              :label "Another Form"
                                              :type "email"
                                              :placeholder "name@example.com")))))
    (testing "Generates correct HTML for a simple form with more than one entries"
      (ok (search "for=exampleFormControlInput1" result))
      (ok (search "class=form-label" result))
      (ok (search "type=email" result))
      (ok (search "class=form-control" result))
      (ok (search "id=exampleFormControlInput1" result))
      (ok (search "placeholder=name@example.com" result))
      (ok (search "for=form1" result))
      (ok (search "Another Form" result)))))
