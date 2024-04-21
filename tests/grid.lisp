(defpackage dev.metalisp.sbt/tests/grid
  (:use
   :cl
   :dev.metalisp.sbt/grid
   :rove)
  (:import-from
   :dev.metalisp.sbt/grid
   :make-con-class
   :make-row-class
   :make-col-class
   :breakpoint-class))

(in-package :dev.metalisp.sbt/tests/grid)

(deftest test-make-con-class-no-value
  (testing "Generates an empty string when value is NIL"
    (ok (string= (make-con-class "xs" nil) ""))))

(deftest test-make-con-class-xs
  (testing "Generates correct class for 'xs' breakpoint"
    (ok (string= (make-con-class "xs" t) "container-xs "))))

(deftest test-make-con-class-sm
  (testing "Generates correct class for 'sm' breakpoint"
    (ok (string= (make-con-class "sm" t) "container-sm "))))

(deftest test-make-con-class-md
  (testing "Generates correct class for 'md' breakpoint"
    (ok (string= (make-con-class "md" t) "container-md "))))

(deftest test-make-con-class-lg
  (testing "Generates correct class for 'lg' breakpoint"
    (ok (string= (make-con-class "lg" t) "container-lg "))))

(deftest test-make-row-class-no-value
  (testing "Generates an empty string when value is NIL"
    (ok (string= (make-row-class "xs" nil) ""))))

(deftest test-make-row-class-general-cols
  (testing "Generates correct class for 'cols' with 2 columns"
    (ok (string= (make-row-class "cols" 2) "row-cols-2 "))))

(deftest test-make-row-class-xs
  (testing "Generates correct class for 'xs' breakpoint with 2 columns"
    (ok (string= (make-row-class "xs" 2) "row-cols-xs-2 "))))

(deftest test-make-row-class-md
  (testing "Generates correct class for 'md' breakpoint with 3 columns"
    (ok (string= (make-row-class "md" 3) "row-cols-md-3 "))))

(deftest test-make-row-class-lg
  (testing "Generates correct class for 'lg' breakpoint with 4 columns"
    (ok (string= (make-row-class "lg" 4) "row-cols-lg-4 "))))

(deftest test-make-col-class-no-size-no-offset
  (testing "Generates an empty string when both size and offset are NIL"
    (ok (string= (make-col-class "xs" '(nil nil)) ""))))

(deftest test-make-col-class-size-only
  (testing "Generates correct class for 'md' breakpoint with size 3 and no offset"
    (ok (string= (make-col-class "md" '(3 nil)) "col-md-3 "))))

(deftest test-make-col-class-offset-only
  (testing "Generates correct class for 'md' breakpoint with no size and offset 2"
    (ok (string= (make-col-class "md" '(nil 2)) "offset-md-2 "))))

(deftest test-make-col-class-size-and-offset
  (testing "Generates correct class for 'lg' breakpoint with size 4 and offset 1"
    (ok (string= (make-col-class "lg" '(4 1)) "col-lg-4 offset-lg-1 "))))

(deftest test-make-col-class-no-name
  (testing "Generates correct class when no name is provided, with size 2 and offset 1"
    (ok (string= (make-col-class "" '(2 1)) "col-2 offset-1 "))))

(deftest test-breakpoint-class-con
  (testing "Generates correct class for container"
    (ok (string= (breakpoint-class :kind "con" :md t) "container-md"))))

(deftest test-breakpoint-class-row
  (testing "Generates correct class for row"
    (ok (string= (breakpoint-class :kind "row" :sm 2 :md 3) "row-cols-sm-2 row-cols-md-3"))))

(deftest test-breakpoint-class-col
  (testing "Generates correct class for column"
    (ok (string= (breakpoint-class :kind "col" :lg '(4 1)) "col-lg-4 offset-lg-1"))))

(deftest test-breakpoint-class-con-fluid
  (testing "Generates correct class for fluid container"
    (ok (string= (breakpoint-class :kind "con" :xs t :sm t :md t :lg t :xl t :xxl t) "container-xs container-sm container-md container-lg container-xl container-xxl"))))

(deftest test-breakpoint-class-col-no-offset
  (testing "Generates correct class for column with no offset"
    (ok (string= (breakpoint-class :kind "col" :md '(3)) "col-md-3"))))

(deftest test-breakpoint-class-no-arguments
  (testing "Asserts error is signaled for no arguments"
    (ok (signals (breakpoint-class)))))

(deftest test-con-fluid
  (let ((result (spinneret:with-html-string (con (:fluid t)))))
    (testing "Generates correct HTML for fluid container"
      (ok (search "<div class=container-fluid></div>" result)))))

(deftest test-con-breakpoint
  (let ((result (spinneret:with-html-string (con (:breakpoint (:kind "con" :md t))))))
    (testing "Generates correct HTML for container with breakpoint"
      (ok (search "<div class=\"container container-md\"></div>" result)))))

(deftest test-con-text
  (let ((result (with-output-to-string (spinneret:*html*) (con (:text (:alignment "center"))))))
    (testing "Generates correct HTML for container with text utilities"
      (ok (search "<div class=\"container text-center\"></div>" result)))))

(deftest test-con-fluid-breakpoint-text
  (let ((result (spinneret:with-html-string (con (:fluid t :breakpoint (:kind "con" :sm t) :text (:weight "bold"))))))
    (testing "Generates correct HTML for fluid container with breakpoint and text utilities"
      (ok (search "<div class=\"container-fluid container-sm fw-bold\"></div>" result)))))

(deftest test-con-no-arguments
  (let ((result (spinneret:with-html-string (con ()))))
    (testing "Generates correct HTML for container with no arguments"
      (ok (search "<div class=container></div>" result)))))

(deftest test-row-cols
  (let ((result (spinneret:with-html-string (row (:cols 2)))))
    (testing "Generates correct HTML when cols is provided"
      (ok (string= result "<div class=\"row row-cols-2\"></div>")))))

(deftest test-row-breakpoint
  (let ((result (spinneret:with-html-string (row (:breakpoint (:kind "row" :sm 2))))))
    (testing "Generates correct HTML when breakpoint is provided"
      (ok (string= result "<div class=\"row row-cols-sm-2\"></div>")))))

(deftest test-row-align-items
  (let ((result (spinneret:with-html-string (row (:alignitems "center")))))
    (testing "Generates correct HTML when align-items is provided"
      (ok (string= result "<div class=\"row align-items-center\"></div>")))))

(deftest test-row-justify-content
  (let ((result (spinneret:with-html-string (row (:justifycontent "between")))))
    (testing "Generates correct HTML when justify-content is provided"
      (ok (string= result "<div class=\"row justify-content-between\"></div>")))))

(deftest test-row-spacing
  (let ((result (spinneret:with-html-string (row (:spacing (:property "m" :size 2))))))
    (testing "Generates correct HTML when spacing is provided"
      (ok (string= result "<div class=\"row m-2\"></div>")))))

(deftest test-row-no-args
  (let ((result (spinneret:with-html-string (row ()))))
    (testing "Generates correct HTML when no arguments are provided"
      (ok (string= result "<div class=row></div>")))))

(deftest test-row-null-cols
  (let ((result (spinneret:with-html-string (row (:cols nil)))))
    (testing "Generates correct HTML when row is null"
      (ok (string= result "<div class=row></div>")))))

(deftest test-col-breakpoint
  (let ((result (spinneret:with-html-string (col (:breakpoint (:kind "col" :md (8 2)))))))
    (testing "Generates correct HTML for column with breakpoint"
      (ok (string= result "<div class=\"col col-md-8 offset-md-2\"></div>")))))

(deftest test-col-align-self
  (let ((result (spinneret:with-html-string (col (:alignself "center")))))
    (testing "Generates correct HTML for column with align-self center"
      (ok (string= result "<div class=\"col align-self-center\"></div>")))))

(deftest test-col-spacing
  (let ((result (spinneret:with-html-string (col (:spacing (:property "p" :size 2))))))
    (testing "Generates correct HTML for column with padding on all sides"
      (ok (string= result "<div class=\"col p-2\"></div>")))))

(deftest test-col-no-arguments
  (let ((result (spinneret:with-html-string (col ()))))
    (testing "Generates correct HTML for column with no arguments"
      (ok (string= result "<div class=col></div>")))))

(deftest test-col-default
  (let ((result (spinneret:with-html-string (col (:cols 3)))))
    (testing "Generates correct HTML for column with no arguments"
      (ok (string= result "<div class=col-3></div>")))))
