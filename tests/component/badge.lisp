(defpackage cl-sbt/tests/badge
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/badge
   :badge
   :badge-primary
   :badge-pill-primary))

(in-package :cl-sbt/tests/badge)

(deftest test-badge-macro
  (testing "Testing badge macro"
    (let ((result (spinneret:with-html-string (badge (:role "primary")))))
      (ok (search "class=\"badge text-bg-primary\"" result)))))

(deftest test-badge-primary
  (testing "Testing badge primary"
    (let ((result (spinneret:with-html-string (badge-primary "foo"))))
      (ok (search "class=\"badge text-bg-primary\"" result)))))

(deftest test-badge-pill-primary
  (testing "Testing badge pill primary"
    (let ((result (spinneret:with-html-string (badge-pill-primary "foo"))))
      (ok (search "class=\"badge text-bg-primary rounded-pill\"" result)))))
