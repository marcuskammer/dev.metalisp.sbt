(in-package :cl-sbt)

(defmacro sbt-btn ((&key (type "default") (size nil)) &body body)
  `(spinneret:with-html
     (:button :type "button"
              :class (concatenate 'string "btn"
                                  (if ,type (format nil " btn-~a" ,type))
                                  (if ,size (format nil " btn-~a" ,size)))
              ,@body)))
