(defpackage dev.metalisp.sbt/tests/main
  (:use :cl
        :dev.metalisp.sbt
        :rove))
(in-package :dev.metalisp.sbt/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :dev.metalisp.sbt)' in your Lisp.

(deftest test-with-page
  (let ((result (with-output-to-string (spinneret:*html*)
                  (with-page (:title "Example")
                    (:h2 "Hello World")))))
    (testing "Testing with-page macro"
      (ok (search "<title>Example</title>" result))
      (ok (search "<h2>Hello World</h2>" result)))))
