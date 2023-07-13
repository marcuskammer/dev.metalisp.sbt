(in-package :cl-sbt-album)

(defmacro footer (&body body)
  `(spinneret:with-html
     (:footer :class "text-body-secondary py-5"
       (:div :class "container"
             (:p :class "float-end mb-1"
                 (:a :href "#" "Back to top"))
             (:p :class "mb-0"
                 ,@body)))))
