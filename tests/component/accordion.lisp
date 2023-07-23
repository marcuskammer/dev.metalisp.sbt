(defpackage cl-sbt/tests/accordion
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/accordion
   :header
   :collapse))

(in-package :cl-sbt/tests/accordion)

(deftest test-header
  (let ((result (spinneret:with-html-string  (header "collapseOne" "Heading" t))))
    (testing "Generates correct HTML for accordion header"
      (ok (string= result
"<h2 class=accordion-header>
 <button class=accordion-button type=button data-bs-toggle=collapse
         data-bs-target=#collapseOne aria-expanded=true
         aria-controls=#collapseOne>Heading</button>
</h2>")))))

(deftest test-collapse
  (let ((result (spinneret:with-html-string  (collapse "accordionExample" "collapseOne" t))))
    (testing "Generates correct HTML for accordion collapse"
      (ok (string= result
"<div class=\"accordion-collapse collapse show\" id=collapseOne
     data-bs-parent=#accordionExample>
 <div class=accordion-body></div>
</div>")))))
