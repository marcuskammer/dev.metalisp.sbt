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

(deftest test-accordion
  (let ((result (spinneret:with-html-string
                  (accordion "accordionExample"
                             (:target "collapseOne" :name "Accordion Item #1" :show t :content "This is the first item's accordion body.")
                             (:target "collapseTwo" :name "Accordion Item #2" :content "This is the second item's accordion body.")
                             (:target "collapseThree" :name "Accordion Item #3" :content "This is the third item's accordion body.")))))
    (testing "Generates correct HTML for accordion"
      (ok (search "class=accordion" result))
      (ok (search "id=accordionExample" result))
      (ok (search "class=accordion-item" result))
      (ok (search "class=accordion-header" result))
      (ok (search "class=accordion-button" result))
      (ok (search "class=\"accordion-collapse collapse show\"" result))
      (ok (search "data-bs-target=#collapseOne" result))
      (ok (search "aria-expanded=true" result))
      (ok (search "aria-controls=#collapseOne" result))
      (ok (search "This is the first item&#39;s accordion body." result))
      (ok (search "This is the second item&#39;s accordion body." result))
      (ok (search "This is the third item&#39;s accordion body." result)))))
