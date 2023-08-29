(defpackage cl-sbt/tests/questionnaire
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/questionnaire
   :question
   :resolve-input-type
   :resolve-input-and-choices
   ::submit-lang
   :questionnaire))

(in-package :cl-sbt/tests/questionnaire)

(deftest test-resolve-input-type
  (testing "Test for resolve-input-type"
    (ok (string= "radio" (resolve-input-type "single")))
    (ok (string= "checkbox" (resolve-input-type "multiple")))
    (ok (string= "text" (resolve-input-type "text")))))

(deftest test-resolve-input-and-choices
  (testing "Test for resolve-input-and-choices"
    (multiple-value-bind (type choices)
        (resolve-input-and-choices '(:radio "A" "B"))
      (ok (string= type "radio"))
      (ok (equal choices '("A" "B"))))
    (multiple-value-bind (type choices)
        (resolve-input-and-choices '("A" "B"))
      (ok (null type))
      (ok (equal choices '("A" "B"))))))
