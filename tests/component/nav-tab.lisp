(defpackage cl-sbt/tests/nav
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/nav
   :item
   :nav))

(in-package :cl-sbt/tests/nav)

(deftest test-item
  (let ((result (spinneret:with-html-string (item "foo" t "#"))))
    (testing "Generate correct HTML nav item"
      (ok (search "class=nav-item" result))
      (ok (search "class=\"nav-link active\"" result)))))
