(in-package :cl-sbt)

(defmacro dropdown ((&key (title "")) &body body)
  `(spinneret:with-html
     (:div :class "dropdown"
           (:button :class "btn btn-secondary dropdown-toggle"
                    :type "button"
                    :data-bs-toggle "dropdown"
                    :aria-expanded "false"
                    ,title)
           ,@body)))

(defmacro dropdown-menu (&body body)
  `(spinneret:with-html
     (:ul :class "dropdown-menu"
          ,@body)))

(defmacro dropdown-item (&body body)
  `(spinneret:with-html
     (:li (:a :class "dropdown-item" :href "#" ,@body))))
