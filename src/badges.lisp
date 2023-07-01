(in-package :cl-sbt)

(defmacro badge ((&key (classes nil)) &body body)
  `(spinneret:with-html
     (:span :class (concatenate 'string "badge"
                                (if ,classes (format nil " ~a" ,classes)))
            ,@body)))
