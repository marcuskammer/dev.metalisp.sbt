(defpackage cl-sbt/tests/alert
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/alert
   :btn
   :alert
   :alert-primary
   :alert-dismiss-primary))

(in-package :cl-sbt/tests/alert)

(deftest test-btn-macro
  (testing "Testing btn macro"
    (let ((result (spinneret:with-html-string (btn))))
      (ok (search "class=btn-close" result))
      (ok (search "type=button" result))
      (ok (search "data-bs-dismiss=alert" result))
      (ok (search "aria-label=Close" result)))))
