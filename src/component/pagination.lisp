;; https://getbootstrap.com/docs/5.3/components/pagination/

;; Bootstrap Pagination is a component that provides an interface for
;; navigating through different pages of content. It is typically used when
;; there is a large amount of data to display that has been split across
;; multiple pages to make it easier to view.

;; The Pagination component includes various classes for styling and
;; customizing the appearance of the pagination links. These include options
;; for displaying the pagination items as links or buttons, marking a
;; pagination item as active (i.e., the current page), and disabling a
;; pagination item.

;; Each individual page is represented as an item in the Pagination component.
;; You can also include "Previous" and "Next" buttons to help users navigate
;; through the pages.

(defpackage cl-sbt-pagination
  (:use :cl)
  (:export
   :item
   :pagination))

(in-package :cl-sbt-pagination)

(defmacro item ((&key (url "#") (active nil)) &body body)
  "This macro generates a Bootstrap pagination item.

URL: The URL to which the item should link. Defaults to '#'.
ACTIVE: Specifies whether the item should be marked as active. If true, the item will be marked as active.
BODY: The contents of the item.

Example usage:
(item (:url \"#\" :active t) \"1\")
This will create an active pagination item with the number '1'."
  `(spinneret:with-html
     (:li :class ,(concatenate 'string "page-item" (if (null active) nil " active"))
          (:a :class "page-link"
              :href ,url
              ,@body))))

(defmacro pagination ((&optional (size nil)) &rest rest)
  "This macro generates a Bootstrap pagination navigation bar.

SIZE: Specifies the size of the pagination bar. Can be 'lg' for large, 'sm' for small, or nil for default size.
REST: A list of pagination items. Each item is a plist with the following keys:
- :name: Specifies the name of the item.
- :url: Specifies the URL to which the item should link.
- :active: Specifies whether the item should be marked as active.

Example usage:
(pagination (:name \"1\" :url \"#\" :active t)
            (:name \"2\" :url \"#\" :active nil)
            (:name \"3\" :url \"#\" :active nil))

To create a small-sized pagination:
(pagination (\"sm\")
            (:name \"1\" :url \"#\" :active t)
            (:name \"2\" :url \"#\" :active nil)
            (:name \"3\" :url \"#\" :active nil))

This will create a pagination bar with three items. The first item will be marked as active."
  `(spinneret:with-html
     (:nav :aria-label "Page navigation"
           (:ul :class ,(concatenate 'string "pagination" (if (null size) nil (format nil " pagination-~a" size)))
                ,@(loop for item in rest
                        collect (destructuring-bind (&key name url active) item
                                  `(item (:url ,url :active ,active) ,name)))))))
