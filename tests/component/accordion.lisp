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
