(defpackage dev.metalisp.sbt/tests/accordion
  (:use
   :cl
   :dev.metalisp.sbt/accordion
   :rove))

(in-package :dev.metalisp.sbt/tests/accordion)

(deftest test-accordion-correct-classes
  (let ((result (with-output-to-string (spinneret:*html*) (accordion (:id "accordionExample" :flush t) "Accordion Item #1" "Accordion Content #1" "Accordion Item #2" "Accordion Content #2" "Accordion Item #3" "Accordion Content #3"))))
    (testing "Generates correct HTML for accordion"
      (ok (search "class=accordion" result))
      (ok (search "class=accordion-item" result))
      (ok (search "class=accordion-header" result))
      (ok (search "class=accordion-button" result)))))

(deftest test-accordion-correct-id
  (let ((result (with-output-to-string (spinneret:*html*) (accordion (:id "accordionExample" :flush t) "Accordion Item #1" "Accordion Content #1" "Accordion Item #2" "Accordion Content #2" "Accordion Item #3" "Accordion Content #3"))))
    (testing "Generates correct HTML for accordion"
      (ok (search "id=accordionExample" result))
      (ok (search "id=collapse-accordionExample-1" result))
      (ok (search "id=collapse-accordionExample-2" result)))))

(deftest test-accordion-correct-aria
  (let ((result (with-output-to-string (spinneret:*html*) (accordion (:id "accordionExample" :flush t) "Accordion Item #1" "Accordion Content #1" "Accordion Item #2" "Accordion Content #2" "Accordion Item #3" "Accordion Content #3"))))
    (testing "Generates correct HTML for accordion"
      (ok (search "aria-expanded=true" result))
      (ok (search "aria-expanded=false" result))
      (ok (search "aria-controls=collapse-accordionExample-1" result)))))
