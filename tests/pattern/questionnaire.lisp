(defpackage cl-sbt/tests/questionnaire
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/questionnaire
   :choicep
   :choicesp
   :questionp
   :question
   :resolve-input-type
   :resolve-input-and-choice
   :extract-question-components
   :questionnaire))

(in-package :cl-sbt/tests/questionnaire)

(deftest test-choicep
  (testing "Test for choicep"
    (ok (choicep '(:radio "foo" "bar")))
    (ok (choicep '(:text "foo")))
    (ok (choicep '(:combo "foo" "bar")))
    (ng (choicep '(:radio "foo" :bar)))
    (ng (choicep '("foo" :bar :radio)))
    (ng (choicep '(() nil nil "foo")))))

(deftest test-choicesp
  (testing "Test for choicesp"
    (ok (choicesp '(:radio "foo" :text "bar")))
    (ng (choicesp '("radio" :foo "text" :bar)))
    (ng (choicesp '("foo" "bar")))
    (ng (choicesp '("foo" :bar)))
    (ng (choicesp '(:radio ())))))

(deftest test-questionp
  (testing "Test for questionp"
    (ok (questionp '(:ask "foo" :group "bar" :choices (:radio "foo"))))
    (ok (questionp '(:frage "foo" :gruppe "bar" :auswahl (:radio "foo"))))
    (ng (questionp '(:ask ("foo") :group ("bar") :choices ("radio"))))
    (ng (questionp '("ask" :foo "group" :bar "choices" :radio)))))

(deftest test-resolve-input-type
  (testing "Test for resolve-input-type"
    (ok (string= "radio" (resolve-input-type "single")))
    (ok (string= "checkbox" (resolve-input-type "multiple")))
    (ok (string= "text" (resolve-input-type "text")))))

(deftest test-resolve-input-and-choice
  (testing "Test for resolve-input-and-choices"
    (multiple-value-bind (type choices)
        (resolve-input-and-choice '(:radio "A" "B"))
      (ok (string= type "radio"))
      (ok (equal choices '("A" "B"))))
    (ok (signals (resolve-input-and-choice '("A" "B"))))))

(deftest test-question-radio
  (let ((result (spinneret:with-html-string
                  (question "What is your favorite color?"
                      "favcolor"
                    (:radio "Red" "Green" "Blue")))))
    (testing "Generates correct HTML for question using radio checkable"
      (ok (search "for=group-favcolor-red" result))
      (ok (search "for=group-favcolor-green" result))
      (ok (search "for=group-favcolor-blue" result))
      (ok (search "id=group-favcolor-red" result))
      (ok (search "id=group-favcolor-green" result))
      (ok (search "id=group-favcolor-blue" result))
      (ok (search "type=radio" result))
      (ok (search "What is your favorite color?" result)))))

(deftest test-question-checkbox
  (let ((result (spinneret:with-html-string
                  (question "What is your favorite color?"
                      "favcolor"
                    (:checkbox "Red" "Green" "Blue")))))
    (testing "Generates correct HTML for question using checkbox checkable"
      (ok (search "type=checkbox" result)))))

(deftest test-question-select
  (let ((result (spinneret:with-html-string
                  (question "What is your favorite color?"
                      "favcolor"
                    (:combo "Red" "Green" "Blue")))))
    (testing "Generates correct HTML for question using select"
      (ok (search "select" result))
      (ok (search "option value=red" result))
      (ok (search "option value=green" result))
      (ok (search "option value=blue" result))
      (ok (search "What is your favorite color?" result)))))

(deftest test-extract-question-components
  (testing "Test for extract-question-components"
    (multiple-value-bind (ask1 group1 choices1)
        (extract-question-components '(:ask "What is your favorite color?"
                                       :group "favcolor"
                                       :choices (:radio "Red" "Green" "Blue")))
      (ok (string= ask1 "What is your favorite color?"))
      (ok (string= group1 "favcolor"))
      (ok (equal choices1 '(:radio "Red" "Green" "Blue"))))))

(deftest test-extract-question-components-missing-components
  (testing "Test for extract-question-components with missing components"
    (ok (signals (extract-question-components '(:ask "What is your favorite color?"
                                                :choices (:radio "Red" "Green" "Blue")))))))

(deftest test-extract-question-components-additional-keys
  (testing "Test for extract-question-components with additional keys"
    (ok (signals (extract-question-components '(:ask "What is your favorite color?"
                                                :group "favcolor"
                                                :choices (:radio "Red" "Green" "Blue")
                                                :extra "some-extra-info"))))))

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

(deftest test-create-questionnaire-select
  (let ((result (spinneret:with-html-string
                  (questionnaire "/submit"
                    (:ask "What is your favorite color?"
                     :group "favcolor"
                     :choices (:combo "Red" "Green" "Blue"))))))
    (ok (search "select" result))
    (ok (search "option value=red" result))
    (ok (search "option value=green" result))
    (ok (search "option value=blue" result))
    (ok (search "What is your favorite color?" result))))
