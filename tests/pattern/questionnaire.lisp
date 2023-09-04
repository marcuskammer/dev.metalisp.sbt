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

(deftest test-create-questionnaire
  (let ((result (spinneret:with-html-string
                   (questionnaire "/submit"
                                  (:ask "How old are you?"
                                   :group "age"
                                   :choices (:single "18-24" "25-34" "35-44" "45-54" "55+"))
                                  (:ask "Your Gender?"
                                   :group "gender"
                                   :choices (:single "Male" "Female" "Non-Binary" "Prefer not to say"))))))
    (testing "Generates correct HTML for questionnaire"
      (ok (search "<form class=py-5 action=/submit method=post>" result))
      (ok (search "<legend>How old are you?</legend>" result))
      (ok (search "<input type=radio name=group-age value=18_24> 18-24</label>" result))
      (ok (search "<input type=radio name=group-gender value=Male> Male</label>" result))
      (ok (search "<hr class=my-4>" result))
      (ok (search "<button class=\"btn btn-primary\" type=submit>Submit</button>" result)))))
