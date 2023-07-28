(defpackage cl-sbt/tests/card
  (:use
   :cl
   :cl-sbt
   :rove)
  (:import-from
   :cl-sbt/card
   :title
   :subtitle
   :text
   :link
   :header
   :img
   :body
   :card-with-img
   :card
   :card-group))

(in-package :cl-sbt/tests/card)

(deftest test-title
  (let ((result (spinneret:with-html-string (title "My Title"))))
    (testing "Testing title macro"
      (ok (string= "<h5 class=card-title>My Title</h5>" result)))))

(deftest test-subtitle
  (let ((result (spinneret:with-html-string (subtitle "My Subtitle"))))
    (testing "Testing subtitle macro"
      (ok (string= "<h6 class=\"card-subtitle mb-2 text-body-secondary\">My Subtitle</h6>" result)))))

(deftest test-text
  (let ((result (spinneret:with-html-string (text "Some card text here"))))
    (testing "Testing text macro"
      (ok (string= "<p class=card-text>Some card text here" result)))))

(deftest test-link
  (let ((result (spinneret:with-html-string (link (:href "https://example.com") "Example link"))))
    (testing "Testing link macro"
      (ok (string= "<a class=card-link href=https://example.com>Example link</a>" result)))))

(deftest test-header
  (let ((result (spinneret:with-html-string (header "My Card Header"))))
    (testing "Testing header macro"
      (ok (search "class=header" result))
      (ok (search "My Card Header" result)))))

(deftest test-img
  (let ((result (spinneret:with-html-string (img (:src "https://example.com/image.jpg" :alt "An example image")))))
    (testing "Testing img macro"
      (ok (string= "<img src=\"https://example.com/image.jpg\" alt=\"An example image\" class=\"card-img-top\" />" result)))))

(deftest test-body
  (let ((result (spinneret:with-html-string (body (title "My Title") (text "Some card text here")))))
    (testing "Testing body macro"
      (ok (string= "<div class=\"card-body\"><h5 class=\"card-title\">My Title</h5><p class=\"card-text\">Some card text here</p></div>" result)))))