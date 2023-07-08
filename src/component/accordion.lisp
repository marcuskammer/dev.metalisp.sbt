;; https://getbootstrap.com/docs/5.3/components/accordion/

(in-package :cl-sbt)

(defmacro accordion-header (target name)
  `(spinneret:with-html
     (:h2 :class "accordion-header"
          (:button :class "accordion-button"
                   :type "button"
                   :data-bs-toggle "collapse"
                   :data-bs-target (format nil "#~a" ,target)
                   :aria-expanded "true"
                   :aria-controls "collapseOne"
                   ,name))))

(defmacro accordion-item (&body body)
  `(spinneret:with-html
     (:div :class "accordion-item"
           ,@body)))

(defmacro accordion ((&key (id "accordionExample")) &rest rest)
  `(spinneret:with-html
     (:div :class "accordion"
           :id ,id
           ,@(loop for item in rest
                   collect (destructuring-bind (&key target name) item
                             `(accordion-item (accordion-header ,target ,name)))))))
