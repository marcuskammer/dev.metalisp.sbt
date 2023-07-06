(in-package :cl-sbt-album)

(defmacro card (&body body)
  `(spinneret:with-html
     (:div :class "col"
           (:div :class "card shadow-sm"
                 (:div :class "card-body"
                       (:p :class "card-text"
                           ,@body))))))
