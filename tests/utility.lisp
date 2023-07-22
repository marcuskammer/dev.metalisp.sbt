(defpackage cl-sbt/tests/utility
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/utility
   :background
   :color
   :opacity
   :overflow
   :sizing
   :spacing
   :text
   :valign))

(in-package :cl-sbt/tests/utility)

(deftest test-background-color
  (testing "Generates correct background class with color"
    (ok (string= (background :color :primary) "bg-primary"))
    (ok (string= (background :color :danger) "bg-danger"))
    (ok (string= (background :color :light) "bg-light"))
    (ok (string= (background :color :dark) "bg-dark"))))

(deftest test-background-gradient
  (testing "Generates correct background class with gradient"
    (ok (string= (background :color :primary :gradient :t) "bg-primary bg-gradient"))
    (ok (string= (background :color :danger :gradient :t) "bg-danger bg-gradient"))
    (ok (string= (background :color :light :gradient :t) "bg-light bg-gradient"))
    (ok (string= (background :color :dark :gradient :t) "bg-dark bg-gradient"))))

(deftest test-background-no-arguments
  (testing "Generates correct background class with no arguments"
    (ok (string= (background) ""))))

(deftest test-color-text
  (testing "Generates correct color class for text"
    (ok (string= (color :text :primary) "text-primary"))
    (ok (string= (color :text :danger) "text-danger"))
    (ok (string= (color :text :light) "text-light"))
    (ok (string= (color :text :dark) "text-dark"))))

(deftest test-color-background
  (testing "Generates correct color class for background"
    (ok (string= (color :background :primary) "bg-primary"))
    (ok (string= (color :background :danger) "bg-danger"))
    (ok (string= (color :background :light) "bg-light"))
    (ok (string= (color :background :dark) "bg-dark"))))

(deftest test-color-text-background
  (testing "Generates correct color class for text and background"
    (ok (string= (color :text :info :background :dark) "text-info bg-dark"))
    (ok (string= (color :text :white :background :primary) "text-white bg-primary"))))

(deftest test-color-no-arguments
  (testing "Generates correct color class with no arguments"
    (ok (string= (color) ""))))

(deftest test-opacity-level
  (testing "Generates correct opacity class for level"
    (ok (string= (opacity :level 0) "opacity-0"))
    (ok (string= (opacity :level 25) "opacity-25"))
    (ok (string= (opacity :level 50) "opacity-50"))
    (ok (string= (opacity :level 75) "opacity-75"))
    (ok (string= (opacity :level 100) "opacity-100"))))

(deftest test-opacity-auto
  (testing "Generates correct opacity class for auto"
    (ok (string= (opacity :level :auto) "opacity-auto"))))

(deftest test-opacity-no-arguments
  (testing "Generates correct opacity class with no arguments"
    (ok (string= (opacity) ""))))

(deftest test-overflow-direction-value
  (testing "Generates correct overflow class for direction and value"
    (ok (string= (overflow :direction :x :value :auto) "overflow-x-auto"))
    (ok (string= (overflow :direction :y :value :hidden) "overflow-y-hidden"))
    (ok (string= (overflow :direction :x :value :visible) "overflow-x-visible"))
    (ok (string= (overflow :direction :y :value :scroll) "overflow-y-scroll"))))

(deftest test-overflow-direction
  (testing "Generates correct overflow class for direction only"
    (ok (string= (overflow :direction :x) "overflow-x"))
    (ok (string= (overflow :direction :y) "overflow-y"))))

(deftest test-overflow-value
  (testing "Generates correct overflow class for value only"
    (ok (string= (overflow :value :auto) "overflow-auto"))
    (ok (string= (overflow :value :hidden) "overflow-hidden"))
    (ok (string= (overflow :value :visible) "overflow-visible"))
    (ok (string= (overflow :value :scroll) "overflow-scroll"))))

(deftest test-overflow-no-arguments
  (testing "Generates correct overflow class with no arguments"
    (ok (string= (overflow) ""))))

(deftest test-sizing-direction-size
  (testing "Generates correct sizing class for direction and size"
    (ok (string= (sizing :direction :w :size 50) "w-50"))
    (ok (string= (sizing :direction :h :size :auto) "h-auto"))
    (ok (string= (sizing :direction :w :size :100) "w-100"))
    (ok (string= (sizing :direction :h :size 75) "h-75"))))

(deftest test-sizing-direction
  (testing "Generates correct sizing class for direction only"
    (ok (string= (sizing :direction :w) "w-"))
    (ok (string= (sizing :direction :h) "h-"))))

(deftest test-sizing-size
  (testing "Generates correct sizing class for size only"
    (ok (string= (sizing :size 50) "50"))
    (ok (string= (sizing :size :auto) "auto"))
    (ok (string= (sizing :size :100) "100"))
    (ok (string= (sizing :size 75) "75"))))

(deftest test-sizing-no-arguments
  (testing "Generates correct sizing class with no arguments"
    (ok (string= (sizing) ""))))

(deftest test-spacing-all-arguments
  (testing "Generates correct spacing class with all arguments"
    (ok (string= (spacing :property :m :side :t :size 3 :breakpoint :md) "mt-md-3"))
    (ok (string= (spacing :property :p :side :b :size 2 :breakpoint :lg) "pb-lg-2"))))

(deftest test-spacing-some-arguments
  (testing "Generates correct spacing class with some arguments"
    (ok (string= (spacing :property :m :size :auto) "m-auto"))
    (ok (string= (spacing :property :p :side :x :size 5) "px-5"))))

(deftest test-spacing-single-argument
  (testing "Generates correct spacing class with a single argument"
    (ok (string= (spacing :property :m) "m"))
    (ok (string= (spacing :side :t) "t"))
    (ok (string= (spacing :size 3) "3"))
    (ok (string= (spacing :breakpoint :md) "md"))))

(deftest test-spacing-no-arguments
  (testing "Generates correct spacing class with no arguments"
    (ok (string= (spacing) ""))))

(deftest test-text-all-arguments
  (testing "Generates correct text utility class with all arguments"
    (ok (string= (text :alignment :center :transform :uppercase :weight :bolder :wrap :nowrap :monospace t)
                 "text-center text-uppercase fw-bolder text-nowrap font-monospace"))))

(deftest test-text-some-arguments
  (testing "Generates correct text utility class with some arguments"
    (ok (string= (text :alignment :start) "text-start"))
    (ok (string= (text :transform :uppercase) "text-uppercase"))
    (ok (string= (text :weight :bold :monospace t) "fw-bold font-monospace"))
    (ok (string= (text :alignment :center :transform :lowercase) "text-center text-lowercase"))
    (ok (string= (text :alignment :end :weight :light :monospace t) "text-end fw-light font-monospace"))
    (ok (string= (text :transform :capitalize :wrap :wrap) "text-capitalize text-wrap"))))

(deftest test-text-single-argument
  (testing "Generates correct text utility class with a single argument"
    (ok (string= (text :alignment :start) "text-start"))
    (ok (string= (text :transform :uppercase) "text-uppercase"))
    (ok (string= (text :weight :bold) "fw-bold"))
    (ok (string= (text :wrap :wrap) "text-wrap"))
    (ok (string= (text :monospace t) "font-monospace"))))

(deftest test-text-no-arguments
  (testing "Generates correct text utility class with no arguments"
    (ok (string= (text) ""))))

(deftest test-valign-all-arguments
  (testing "Generates correct vertical align class with all arguments"
    (ok (string= (valign :align :baseline) "align-baseline"))
    (ok (string= (valign :align :top) "align-top"))
    (ok (string= (valign :align :middle) "align-middle"))
    (ok (string= (valign :align :bottom) "align-bottom"))
    (ok (string= (valign :align :text-bottom) "align-text-bottom"))
    (ok (string= (valign :align :text-top) "align-text-top"))))

(deftest test-valign-no-arguments
  (testing "Generates correct vertical align class with no arguments"
    (ok (string= (valign) ""))))
