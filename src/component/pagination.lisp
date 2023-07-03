(in-package :cl-sbt)

(defmacro pagination (&body body)
  `(spinneret:with-html
     (:nav :aria-label "Page navigation example"
           (:ul :class "pagination"
                ,@body))))

(defmacro pagination-with-icons (&body body)
  `(spinneret:with-html
     (:nav :aria-label "Page navigation example"
           (:ul :class "pagination"
                (:li :class "page-item"
                     (:a :class "page-link"
                         :href "#"
                         :aria-label "Previous"
                         (:span :aria-hidden "true" "&laquo;")))
                ,@body
                (:li :class "page-item"
                     (:a :class "page-link"
                         :href "#"
                         :aria-label "Next"
                         (:span :aria-hidden "true" "&raquo;")))))))
