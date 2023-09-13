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

(deftest test-create-questionnaire-single
  (let ((result (spinneret:with-html-string
                   (questionnaire "/submit"
                                  (:ask "Your Gender?"
                                   :group "gender"
                                   :choices (:single "Male" "Female" "Non-Binary" "Prefer not to say"))))))
    (testing "Generates correct HTML for questionnaire with single choices"
      (ok (search "<form class=py-5 action=/submit method=post>" result))
      (ok (search "<legend>Your Gender?</legend>" result))
      (ok (search "class=form-check-input" result))
      (ok (search "name=group-gender" result))
      (ok (search "value=male" result))
      (ok (search "<hr class=my-4>" result))
      (ok (search "<button class=\"btn btn-primary\" type=submit>Submit</button>" result)))))

(deftest test-create-questionnaire-multiple
  (let ((result (spinneret:with-html-string
                   (questionnaire "/submit"
                                  (:ask "Which of the following devices do you regularly use to browse the internet?"
                                   :group "device"
                                   :choices (:multiple "Desktop" "Laptop" "Tablet"))))))
    (testing "Generates correct HTML for questionnaire with multiple choices"
      (ok (search "<form class=py-5 action=/submit method=post>" result))
      (ok (search "<legend>Which of the following devices do you regularly use to browse the internet?</legend>" result))
      (ok (search "type=checkbox" result))
      (ok (search "name=group-device" result))
      (ok (search "value=desktop" result))
      (ok (search "<hr class=my-4>" result))
      (ok (search "<button class=\"btn btn-primary\" type=submit>Submit</button>" result)))))

(deftest test-create-questionnaire-mixed-choices
  (let ((result (spinneret:with-html-string
                   (questionnaire "/submit"
                                  (:ask "Which of the following devices do you regularly use to browse the internet?"
                                   :group "device"
                                   :choices (:multiple "Desktop" "Laptop" "Tablet"
                                             :text "Others (please specify)"))))))
    (testing "Generates correct HTML for questionnaire with multiple choices"
      (ok (search "<form class=py-5 action=/submit method=post>" result))
      (ok (search "<legend>Which of the following devices do you regularly use to browse the internet?</legend>" result))
      (ok (search "type=checkbox" result))
      (ok (search "name=group-device" result))
      (ok (search "class=\"form-label group-device\"" result))
      (ok (search "class=form-control" result))
      (ok (search "<hr class=my-4>" result))
      (ok (search "<button class=\"btn btn-primary\" type=submit>Submit</button>" result)))))

(deftest test-create-questionnaire-single-german
  (let* ((spinneret:*html-lang* "de")
         (result (spinneret:with-html-string
                   (questionnaire "/submit"
                                  (:frage "Ihr Geschlecht?"
                                   :gruppe "geschlecht"
                                   :auswahl (:single "Männlich"
                                                     "Weiblich"
                                                     "Non-Binary"
                                                     "Keine Angabe"))))))
    (testing "Generates correct HTML for questionnaire with single choices"
      (ok (search "<form class=py-5 action=/submit method=post>" result))
      (ok (search "<legend>Ihr Geschlecht?</legend>" result))
      (ok (search "class=form-check-input" result))
      (ok (search "name=group-geschlecht" result))
      (ok (search "value=männlich" result))
      (ok (search "value=weiblich" result))
      (ok (search "value=keine-angabe" result))
      (ok (search "<button class=\"btn btn-primary\" type=submit>Absenden</button>" result)))))
