(defpackage dev.metalisp.sbt/tests/component/nav
  (:use
   :cl
   :dev.metalisp.sbt/component/nav
   :rove))

(in-package :dev.metalisp.sbt/tests/component/nav)

(deftest test-item
  (let ((result (spinneret:with-html-string (item "foo" t "#"))))
    (testing "Generate correct HTML nav item"
      (ok (search "class=nav-item" result))
      (ok (search "class=\"nav-link active\"" result)))))
