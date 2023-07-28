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
    (let ((result (spinneret:with-html-string (btn (:type "primary") "foo"))))
      (ok (search "class=\"btn btn-primary\"" result)))))

(deftest test-btn-primary
  (testing "Testing btn primary"
    (let ((result (spinneret:with-html-string (btn-primary "foo"))))
      (ok (search "class=\"btn btn-primary\"" result)))))

(deftest test-btn-outline-primary
  (testing "Testing btn outline primary"
    (let ((result (spinneret:with-html-string (btn-outline-primary "foo"))))
      (ok (search "class=\"btn btn-outline-primary\"" result)))))
