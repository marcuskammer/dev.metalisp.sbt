;; https://getbootstrap.com/docs/5.3/components/accordion/

(in-package :cl-sbt)

(defmacro accordion-header (target name show)
  `(spinneret:with-html
     (:h2 :class "accordion-header"
          (:button :class "accordion-button"
                   :type "button"
                   :data-bs-toggle "collapse"
                   :data-bs-target (format nil "#~a" ,target)
                   :aria-expanded ,(if show "true" "false")
                   :aria-controls (format nil "#~a" ,target)
                   ,name))))

(defmacro accordion-collapse (parent id show &body body)
  `(spinneret:with-html
     (:div :id ,id
           :class ,(concatenate 'string "accordion-collapse collapse" (when show " show"))
           :data-bs-parent (format nil "#~a" ,parent)
           (:div :class "accordion-body"
                 ,@body))))

(defmacro accordion-item (&body body)
  `(spinneret:with-html
     (:div :class "accordion-item"
           ,@body)))

(defmacro accordion ((&key (id "accordionExample")) &rest rest)
  `(spinneret:with-html
     (:div :class "accordion"
           :id ,id
           ,@(loop for item in rest
                   collect (destructuring-bind (&key target name show content) item
                             `(accordion-item (accordion-header ,target ,name ,show)
                                (accordion-collapse ,id ,target ,show ,content)))))))

(defun show-accordion-example ()
  "Show an generated accordion example"
  (accordion (:id "accordionExample")
             (:target "collapseOne" :name "Accordion Item #1" :show t :content "This is the first item's accordion body.")
             (:target "collapseTwo" :name "Accordion Item #2" :content "This is the second item's accordion body.")
             (:target "collapseThree" :name "Accordion Item #3" :content "This is the second item's accordion body.")))
