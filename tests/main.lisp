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

(deftest test-remove-special-chars
  (testing "Removes all special characters from the string STR except numbers and alphabets."
    (ok (string= (remove-special-chars "a1b!@#$%^&*()c2") "a1bc2"))))

(deftest test-clean-form-str
  (testing "Cleans a form string for use as a name or identifier."
    (ok (string= (clean-form-str " hello WORLD") "hello-world"))))

(deftest test-build-str-name
  (testing "Builds a standardized string by adding a 'group-' prefix and applying cleaning functions."
    (ok (string= (build-str-name "somename") "group-somename"))))

(deftest test-build-str-value
  (testing "Trims leading and trailing spaces from the given value string."
    (ok (string= (build-str-value " hello-world ") "hello-world"))))

(deftest test-build-str-value-prop
  (testing "Builds a value property string by applying various cleaning functions."
    (ok (string= (build-str-value-prop " hello world ") "hello-world"))))
