(defpackage dev.metalisp.sbt/tests/nav
  (:use
   :cl
   :dev.metalisp.sbt
   :rove)
  (:import-from
   :dev.metalisp.sbt/component/nav
   :item
   :nav))

(in-package :dev.metalisp.sbt/tests/nav)

(deftest test-item
  (let ((result (spinneret:with-html-string (item "foo" t "#"))))
    (testing "Generate correct HTML nav item"
      (ok (search "class=nav-item" result))
      (ok (search "class=\"nav-link active\"" result)))))
