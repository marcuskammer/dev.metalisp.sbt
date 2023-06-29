(defpackage cl-sbt/tests/main
  (:use :cl
        :cl-sbt
        :rove))
(in-package :cl-sbt/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-sbt)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
