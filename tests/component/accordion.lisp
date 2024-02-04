(defpackage dev.metalisp.sbt/tests/accordion
  (:use
   :cl
   :dev.metalisp.sbt
   :rove)
  (:import-from
   :dev.metalisp.sbt/component/accordion
   :header
   :collapse
   :item
   :accordion))

(in-package :dev.metalisp.sbt/tests/accordion)

(deftest test-header
  (let ((result (spinneret:with-html-string (header "collapseOne" "Heading" t))))
    (testing "Generates correct HTML for accordion header"
      (ok (search "class=accordion-header" result))
      (ok (search "class=accordion-button" result))
      (ok (search "data-bs-target=#collapseOne" result))
      (ok (search "aria-expanded=true" result))
      (ok (search "aria-controls=#collapseOne" result))
      (ok (search "Heading" result)))))

(deftest test-collapse
  (let ((result (spinneret:with-html-string (collapse "accordionExample" "collapseOne" t))))
    (testing "Generates correct HTML for accordion collapse"
      (ok (search "class=\"accordion-collapse collapse show\"" result))
      (ok (search "id=collapseOne" result))
      (ok (search "data-bs-parent=#accordionExample" result))
      (ok (search "class=accordion-body" result)))))

(deftest test-item
  (let ((result (spinneret:with-html-string (item (header "collapseOne" "Heading" t) (collapse "accordionExample" "collapseOne" t "Some content")))))
    (testing "Generates correct HTML for accordion item"
      (ok (search "class=accordion-item" result))
      (ok (search "class=accordion-header" result))
      (ok (search "class=accordion-button" result))
      (ok (search "class=\"accordion-collapse collapse show\"" result))
      (ok (search "data-bs-target=#collapseOne" result))
      (ok (search "aria-expanded=true" result))
      (ok (search "aria-controls=#collapseOne" result))
      (ok (search "Some content" result)))))

(deftest test-accordion-correct-classes
  (let ((result (with-output-to-string (spinneret:*html*) (accordion (:id "accordionExample" :flush t) ("Accordion Item #1" "Accordion Content #1") ("Accordion Item #2" "Accordion Content #2") ("Accordion Item #3" "Accordion Content #3")))))
    (testing "Generates correct HTML for accordion"
      (ok (search "class=accordion" result))
      (ok (search "class=accordion-item" result))
      (ok (search "class=accordion-header" result))
      (ok (search "class=accordion-button" result)))))

(deftest test-accordion-correct-id
  (let ((result (with-output-to-string (spinneret:*html*) (accordion (:id "accordionExample" :flush t) ("Accordion Item #1" "Accordion Content #1") ("Accordion Item #2" "Accordion Content #2") ("Accordion Item #3" "Accordion Content #3")))))
    (testing "Generates correct HTML for accordion"
      (ok (search "id=accordionExample" result))
      (ok (search "id=collapse-accordionExample-1" result))
      (ok (search "id=collapse-accordionExample-2" result)))))

(deftest test-accordion-correct-aria
  (let ((result (with-output-to-string (spinneret:*html*) (accordion (:id "accordionExample" :flush t) ("Accordion Item #1" "Accordion Content #1") ("Accordion Item #2" "Accordion Content #2") ("Accordion Item #3" "Accordion Content #3")))))
    (testing "Generates correct HTML for accordion"
      (ok (search "aria-expanded=true" result))
      (ok (search "aria-expanded=false" result))
      (ok (search "aria-controls=collapse-accordionExample-1" result)))))
