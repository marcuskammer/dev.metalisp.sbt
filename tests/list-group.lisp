(defpackage dev.metalisp.sbt/tests/component/list-group
  (:use
   :cl
   :dev.metalisp.sbt/component/list-group
   :rove))

(in-package :dev.metalisp.sbt/tests/component/list-group)

(deftest test-item
  (let ((result (spinneret:with-html-string (item "Item 1"))))
    (testing "Generates correct HTML for a list group item"
      (ok (search "class=list-group-item" result))
      (ok (search "Item 1" result)))))

(deftest test-list-group
  (let ((result (spinneret:with-html-string (list-group (:content "Item 1") (:content "Item 2")))))
    (testing "Generates correct HTML for a list group with items"
      (ok (search "class=\"list-group list-group-flush\"" result))
      (ok (search "class=list-group-item" result))
      (ok (search "Item 1" result))
      (ok (search "Item 2" result)))))
