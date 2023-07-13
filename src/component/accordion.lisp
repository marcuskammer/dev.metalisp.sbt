;; https://getbootstrap.com/docs/5.3/components/accordion/

;; The Bootstrap Accordion component is a convenient way to condense a large
;; amount of content into a limited amount of space.

;; An Accordion is a list of headers that can be clicked to hide or reveal
;; additional content associated with them. Each header can control the visibility
;; of a 'collapse' element that can hold any kind of HTML content.

;; Each accordion item (consisting of a header and its associated collapse) can be
;; expanded or collapsed independently of the others. However, in Bootstrap, the
;; Accordion is designed so that only one item can be expanded at a time.

;; The component uses Bootstrap's collapse JavaScript plugin, and it is designed
;; to be accessible and web standards-compliant. This means it's keyboard
;; navigable and it provides appropriate ARIA roles and properties.

;; The Accordion component is perfect for FAQ sections, multi-part forms, or any
;; situation where you need to present the user with a range of options or
;; information, but you want to keep the interface uncluttered by hiding content
;; that isn't immediately necessary.

(defpackage cl-sbt-accordion
  (:use :cl)
  (:export
   :header
   :collapse
   :item
   :accordion))

(in-package :cl-sbt-accordion)

(defmacro header (target name show)
  "This macro generates a Bootstrap header for an accordion item.

   TARGET: (optional) The id of the collapse this header controls. Defaults to 'collapseOne'.

   NAME: (optional) The text that will be displayed on this header. Defaults to 'Heading'.

   SHOW: (optional) A boolean indicating whether the collapse controlled by this header should be shown when
         the accordion first loads. If true, the 'aria-expanded' attribute will be 'true'. Defaults to NIL.

   Example:
     (header \"collapseOne\" \"Heading\" t)"

  `(spinneret:with-html
     (:h2 :class "accordion-header"
          (:button :class "accordion-button"
                   :type "button"
                   :data-bs-toggle "collapse"
                   :data-bs-target (format nil "#~a" ,target)
                   :aria-expanded ,(if (null show) "false" "true")
                   :aria-controls (format nil "#~a" ,target)
                   ,name))))

(defmacro collapse (parent id show &body body)
  "This macro generates a Bootstrap collapse for an accordion item.

   PARENT: The id of the parent element that contains the collapse.

   ID: The unique id for this collapse.

   SHOW: A boolean indicating whether the collapse should be shown when the accordion first loads.
         If true, 'show' will be added to the classes of the collapse.

   BODY: The contents of the collapse.

   Example:
     (collapse \"accordionExample\" \"collapseOne\" t \"Some content\")"

  `(spinneret:with-html
     (:div :id ,id
           :class ,(concatenate 'string "accordion-collapse collapse" (if (null show) nil " show"))
           :data-bs-parent (format nil "#~a" ,parent)
           (:div :class "accordion-body"
                 ,@body))))

(defmacro item (&body body)
  "This macro generates a Bootstrap accordion item.

   BODY: The contents of the accordion item.

   The macro creates a <div> element with the class 'accordion-item' and inserts the BODY as its child elements.

   Example:
     (item (header \"collapseOne\" \"Heading\" t) (collapse \"accordionExample\" \"collapseOne\" t \"Some content\"))"

  `(spinneret:with-html
     (:div :class "accordion-item"
           ,@body)))

(defmacro accordion ((&key (id "accordionExample")) &rest rest)
  "This macro generates an accordion-style collapsible list with Bootstrap.

   ID: Specifies a unique identifier for the accordion. Defaults to 'accordionExample'.

   REST: Specifies a list of accordion items. Each item is a plist with the following keys:
   - :target: Specifies a unique identifier for the accordion item.
   - :name: Specifies the name of the accordion item.
   - :show: Specifies whether the accordion item should be displayed by default.
   - :content: Specifies the content of the accordion item.

   Example:
    (accordion (:id \"accordionExample\")
               (:target \"collapseOne\" :name \"Accordion Item #1\" :show t :content \"This is the first item's accordion body.\")
               (:target \"collapseTwo\" :name \"Accordion Item #2\" :content \"This is the second item's accordion body.\")
               (:target \"collapseThree\" :name \"Accordion Item #3\" :content \"This is the second item's accordion body.\"))"

  `(spinneret:with-html
     (:div :class "accordion"
           :id ,id
           ,@(loop for item in rest
                   collect (destructuring-bind (&key target name show content) item
                             `(item (header ,target ,name ,show)
                                (collapse ,id ,target ,show ,content)))))))

(defun show-accordion-example ()
  "Show an generated accordion example"
  (accordion (:id "accordionExample")
             (:target "collapseOne" :name "Accordion Item #1" :show t :content "This is the first item's accordion body.")
             (:target "collapseTwo" :name "Accordion Item #2" :content "This is the second item's accordion body.")
             (:target "collapseThree" :name "Accordion Item #3" :content "This is the second item's accordion body.")))
