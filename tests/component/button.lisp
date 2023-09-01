(defpackage cl-sbt/tests/btn
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/btn
   :btn
   :btn-primary
   :btn-outline-primary))

(in-package :cl-sbt/tests/btn)

(deftest test-btn-macro
  (testing "Testing btn macro"
    (let ((result (spinneret:with-html-string (btn (:color "primary") "foo"))))
      (ok (search "class=\"btn btn-primary\"" result))
      (ok (search "type=button" result)))))

(deftest test-btn-macro-with-type
  (testing "Testing btn macro with type"
    (let ((result (spinneret:with-html-string (btn (:type "submit" :color "primary") "foo"))))
      (ok (search "class=\"btn btn-primary\"" result))
      (ok (search "type=submit" result)))))

(deftest test-btn-macro-with-id
  (testing "Testing btn macro with id"
    (let ((result (spinneret:with-html-string (btn (:id "submit" :color "primary") "foo"))))
      (ok (search "class=\"btn btn-primary\"" result))
      (ok (search "id=submit" result)))))

(deftest test-btn-primary
  (testing "Testing btn primary"
    (let ((result (spinneret:with-html-string (btn-primary nil "foo"))))
      (ok (search "class=\"btn btn-primary\"" result))
      (ok (search "type=button" result)))))

(deftest test-btn-outline-primary
  (testing "Testing btn outline primary without additional keywords"
    (let ((result (spinneret:with-html-string (btn-outline-primary () "foo"))))
      (ok (search "class=\"btn btn-outline-primary\"" result))
      (ok (search "type=button" result)))))

(deftest test-btn-outline-primary-with-type-submit
  (testing "Testing btn outline primary with type submit"
    (let ((result (spinneret:with-html-string (btn-outline-primary (:type "submit") "foo"))))
      (ok (search "class=\"btn btn-outline-primary\"" result))
      (ok (search "type=submit" result)))))

(deftest test-btn-outline-primary-with-id
  (testing "Testing btn outline primary with id example"
    (let ((result (spinneret:with-html-string (btn-outline-primary (:id "example") "foo"))))
      (ok (search "class=\"btn btn-outline-primary\"" result))
      (ok (search "type=button" result))
      (ok (search "id=example" result)))))
