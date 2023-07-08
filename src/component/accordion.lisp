(in-package :cl-sbt)

(defmacro accordion-header (&body body)
  `(spinneret:with-html
       (:h2 :class "accordion-header"
                  (:button :class "accordion-button"
                           :type "button"
                           :data-bs-toggle="collapse"
                           :data-bs-target="#collapseOne"
                           :aria-expanded="true"
                           :aria-controls="collapseOne"
                           ,@body))))

(defmacro accordion-item (&body body)
    `(spinneret:with-html
       (:div :class "accordion-item"
             ,@body)))

(defmacro accordion ((&key (id nil)) &body body)
    `(spinneret:with-html
       (:div :class "accordion"
             :id ,id
             ,@body)))
