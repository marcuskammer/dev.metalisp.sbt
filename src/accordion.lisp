(in-package :cl-sbt)

(defmacro accordion-item ()
    `(spinneret:with-html
       (:div :class "accordion-item"
             (:h2 :class "accordion-header"
                  (:button :class "accordion-button"
                           :type "button"
                           :data-bs-toggle="collapse"
                           :data-bs-target="#collapseOne"
                           :aria-expanded="true"
                           :aria-controls="collapseOne"
                           "Accordion Item #1")))))

(defmacro accordion ()
    `(spinneret:with-html
       (:div :class "accordion"
             :id "accordionExample"
             (sbt-accordion-item))))
